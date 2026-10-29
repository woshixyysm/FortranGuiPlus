!> Geometry batcher: turns a ui_drawlist into ONE triangle list + one texture atlas.
!>
!> Every GPU backend (Vulkan / OpenGL / GLES / D3D11 / D3D12) therefore needs a
!> single pipeline, a single texture and a single draw call per frame.
!>
!> Vertices are emitted already transformed to normalised device coordinates,
!> so the backends need no uniform buffers, no push constants and no matrices.
!> Y is flipped here (UI space is y-down, NDC is y-up), which reverses the
!> winding order - all backends must disable face culling.
!>
!> Vertex layout (32 bytes, C compatible):
!>    offset  0 : float x, y      clip space, -1..1
!>    offset  8 : float u, v      atlas coordinates
!>    offset 16 : float r, g, b, a  straight alpha, 0..1
module ui_geom
   use, intrinsic :: iso_c_binding, only: c_float
   use ui_kinds
   use ui_draw
   use ui_font
   implicit none
   private

   public :: ui_vertex, ui_batch, ui_vertex_bytes
   public :: UI_ATLAS_W, UI_ATLAS_H, ui_atlas_build
   public :: ui_white_uv, ui_batch_line_px, ui_set_text_sink
   public :: UI_MODE_ATLAS, UI_MODE_GLASS, UI_MODE_BG

   !> Atlas: 16 columns x 6 rows of 8x8 glyph cells, plus one row of solid white
   !> used by every untextured primitive.
   integer, parameter :: UI_ATLAS_W = 128
   integer, parameter :: UI_ATLAS_H = 56
   integer, parameter :: CELL = 8
   integer, parameter :: ATLAS_COLS = 16
   integer, parameter :: WHITE_ROW = 6

   !> UV of the solid white texel (centre of the extra row).
   real(wp), parameter :: ui_white_uv(2) = [0.5_wp/UI_ATLAS_W, &
                                           (real(WHITE_ROW*CELL, wp) + 3.5_wp)/UI_ATLAS_H]

   !> Segments used to tessellate a circle.
   integer, parameter :: CIRCLE_SEGMENTS = 24

   !> Vertex modes understood by the GL fragment shader.
   integer, parameter :: UI_MODE_ATLAS = 0
   integer, parameter :: UI_MODE_GLASS = 1
   integer, parameter :: UI_MODE_BG = 2

   type, bind(c) :: ui_vertex
      real(c_float) :: x
      real(c_float) :: y
      real(c_float) :: u
      real(c_float) :: v
      real(c_float) :: r
      real(c_float) :: g
      real(c_float) :: b
      real(c_float) :: a
      !> 0 = atlas, 1 = frosted backdrop, 2 = blurred backdrop, 3 = background
      real(c_float) :: mode
      !> Glass only: offset from the body centre, and its half size + radius.
      real(c_float) :: lx
      real(c_float) :: ly
      real(c_float) :: hw
      real(c_float) :: hh
      real(c_float) :: rad
   end type ui_vertex

   type :: ui_batch
      type(ui_vertex), allocatable :: v(:)
      integer :: count = 0
      integer :: cap = 0
      integer :: vw = 1
      integer :: vh = 1
      !> Last frosted glass panel: x, y, w, h and corner radius, in UI pixels.
      !> The GL shader turns this into a rounded-box SDF for real refraction.
      real(wp) :: glass_rect(4) = 0.0_wp
      real(wp) :: glass_r = 0.0_wp
   contains
      procedure :: reset => batch_reset
      procedure :: build => batch_build
      procedure :: rect_px => batch_rect_px
   end type ui_batch

   abstract interface
      !> Draws one string into the batch, in UI pixel coordinates.
      subroutine ui_text_sink_iface(b, x, y, s, col, scale)
         import :: ui_batch, ui_color, wp
         class(ui_batch), intent(inout) :: b
         real(wp), intent(in) :: x, y, scale
         character(*), intent(in) :: s
         type(ui_color), intent(in) :: col
      end subroutine ui_text_sink_iface
   end interface

   procedure(ui_text_sink_iface), pointer, save :: g_text_sink => null()

contains

   !> Size of one vertex in bytes, as required by every GPU API.
   integer function ui_vertex_bytes() result(n)
      type(ui_vertex) :: probe

      n = storage_size(probe)/8
   end function ui_vertex_bytes

   !> Rasterise the built-in 5x7 font into an 8-bit coverage atlas.
   subroutine ui_atlas_build(tex)
      integer(ip), intent(out) :: tex(UI_ATLAS_W, UI_ATLAS_H)
      integer :: idx, code, col, row, r, c
      integer(ip) :: bits(7)

      tex = 0_ip
      call ui_font_init()

      do idx = 0, 94
         code = 32 + idx
         col = mod(idx, ATLAS_COLS)
         row = idx/ATLAS_COLS
         bits = ui_font_rows(code)
         do r = 1, 7
            do c = 1, 5
               if (btest(bits(r), 5 - c)) tex(col*CELL + c, row*CELL + r) = 255_ip
            end do
         end do
      end do

      ! Solid white row for flat coloured geometry.
      tex(:, WHITE_ROW*CELL + 1:WHITE_ROW*CELL + CELL) = 255_ip
   end subroutine ui_atlas_build

   !> Install a vector text renderer. The batch then emits real glyph outlines
   !> instead of the 5x7 bitmap atlas, so text stays sharp at any DPI.
   subroutine ui_set_text_sink(f)
      procedure(ui_text_sink_iface) :: f

      g_text_sink => f
   end subroutine ui_set_text_sink

   subroutine batch_reset(self)
      class(ui_batch), intent(inout) :: self

      self%count = 0
   end subroutine batch_reset

   !> Grow the vertex array.
   subroutine batch_grow(self, need)
      class(ui_batch), intent(inout) :: self
      integer, intent(in) :: need
      type(ui_vertex), allocatable :: tmp(:)
      integer :: newcap

      if (need <= self%cap) return
      newcap = max(1024, 2*max(self%cap, 1))
      do while (newcap < need)
         newcap = 2*newcap
      end do
      allocate (tmp(newcap))
      if (self%count > 0) tmp(1:self%count) = self%v(1:self%count)
      call move_alloc(tmp, self%v)
      self%cap = newcap
   end subroutine batch_grow

   !> Append one vertex given pixel coordinates (clipped to NDC on the fly).
   !> `mode` selects what the fragment shader blends the colour with.
   subroutine push_v(self, px, py, u, v, col, mode)
      class(ui_batch), intent(inout) :: self
      real(wp), intent(in) :: px, py, u, v
      type(ui_color), intent(in) :: col
      integer, intent(in), optional :: mode
      integer :: i

      if (self%count + 1 > self%cap) call batch_grow(self, self%count + 1)
      self%count = self%count + 1
      i = self%count
      self%v(i)%x = real(px/real(self%vw, wp)*2.0_wp - 1.0_wp, c_float)
      self%v(i)%y = real(1.0_wp - py/real(self%vh, wp)*2.0_wp, c_float)
      self%v(i)%u = real(u, c_float)
      self%v(i)%v = real(v, c_float)
      self%v(i)%r = real(col%r, c_float)/255.0_c_float
      self%v(i)%g = real(col%g, c_float)/255.0_c_float
      self%v(i)%b = real(col%b, c_float)/255.0_c_float
      self%v(i)%a = real(col%a, c_float)/255.0_c_float
      self%v(i)%mode = 0.0_c_float
      if (present(mode)) self%v(i)%mode = real(mode, c_float)
      self%v(i)%lx = 0.0_c_float
      self%v(i)%ly = 0.0_c_float
      self%v(i)%hw = 0.0_c_float
      self%v(i)%hh = 0.0_c_float
      self%v(i)%rad = 0.0_c_float
   end subroutine push_v

   !> Axis aligned quad with an explicit UV rectangle.
   subroutine push_quad_uv(self, x0, y0, x1, y1, u0, v0, u1, v1, col)
      class(ui_batch), intent(inout) :: self
      real(wp), intent(in) :: x0, y0, x1, y1, u0, v0, u1, v1
      type(ui_color), intent(in) :: col

      call push_v(self, x0, y0, u0, v0, col)
      call push_v(self, x1, y0, u1, v0, col)
      call push_v(self, x1, y1, u1, v1, col)
      call push_v(self, x0, y0, u0, v0, col)
      call push_v(self, x1, y1, u1, v1, col)
      call push_v(self, x0, y1, u0, v1, col)
   end subroutine push_quad_uv

   !> Axis aligned quad using the solid white texel.
   subroutine push_rect(self, x, y, w, h, col)
      class(ui_batch), intent(inout) :: self
      real(wp), intent(in) :: x, y, w, h
      type(ui_color), intent(in) :: col

      if (w <= 0.0_wp .or. h <= 0.0_wp) return
      call push_quad_uv(self, x, y, x + w, y + h, ui_white_uv(1), ui_white_uv(2), &
                        ui_white_uv(1), ui_white_uv(2), col)
   end subroutine push_rect

   !> Arbitrary convex quad from four pixel-space corners.
   subroutine push_quad4(self, p, col)
      class(ui_batch), intent(inout) :: self
      real(wp), intent(in) :: p(2, 4)
      type(ui_color), intent(in) :: col
      real(wp) :: u, v

      u = ui_white_uv(1)
      v = ui_white_uv(2)
      call push_v(self, p(1, 1), p(2, 1), u, v, col)
      call push_v(self, p(1, 2), p(2, 2), u, v, col)
      call push_v(self, p(1, 3), p(2, 3), u, v, col)
      call push_v(self, p(1, 1), p(2, 1), u, v, col)
      call push_v(self, p(1, 3), p(2, 3), u, v, col)
      call push_v(self, p(1, 4), p(2, 4), u, v, col)
   end subroutine push_quad4

   !> One triangle with the white texel.
   subroutine push_tri(self, p, col)
      class(ui_batch), intent(inout) :: self
      real(wp), intent(in) :: p(2, 3)
      type(ui_color), intent(in) :: col
      integer :: k

      do k = 1, 3
         call push_v(self, p(1, k), p(2, k), ui_white_uv(1), ui_white_uv(2), col)
      end do
   end subroutine push_tri

   !> Thick line segment, drawn as a rotated quad.
   subroutine emit_line(self, x0, y0, x1, y1, col, thickness)
      class(ui_batch), intent(inout) :: self
      real(wp), intent(in) :: x0, y0, x1, y1, thickness
      type(ui_color), intent(in) :: col
      real(wp) :: dx, dy, len, nx, ny, h, p(2, 4)

      dx = x1 - x0
      dy = y1 - y0
      len = sqrt(dx*dx + dy*dy)
      if (len <= 0.0_wp) then
         call push_rect(self, x0 - thickness*0.5_wp, y0 - thickness*0.5_wp, &
                        thickness, thickness, col)
         return
      end if
      h = max(thickness, 1.0_wp)*0.5_wp
      nx = -dy/len*h
      ny = dx/len*h

      p(:, 1) = [x0 + nx, y0 + ny]
      p(:, 2) = [x1 + nx, y1 + ny]
      p(:, 3) = [x1 - nx, y1 - ny]
      p(:, 4) = [x0 - nx, y0 - ny]
      call push_quad4(self, p, col)
   end subroutine emit_line

   !> Filled disc or ring, tessellated as a triangle fan / strip of quads.
   subroutine emit_circle(self, cx, cy, radius, col, filled, thickness)
      class(ui_batch), intent(inout) :: self
      real(wp), intent(in) :: cx, cy, radius, thickness
      type(ui_color), intent(in) :: col
      logical, intent(in) :: filled
      integer :: s, nseg
      real(wp) :: a0, a1, r1, tri(2, 3), quad(2, 4), segf

      r1 = max(radius - max(thickness, 1.0_wp), 0.0_wp)

      ! tessellate by radius: a 4 px dot needs few wedges, a 200 px disc many
      segf = max(12.0_wp, min(160.0_wp, 2.2_wp*radius + 8.0_wp))
      nseg = int(segf)

      do s = 0, nseg - 1
         a0 = 6.283185307179586_wp*real(s, wp)/real(nseg, wp)
         a1 = 6.283185307179586_wp*real(s + 1, wp)/real(nseg, wp)
         if (filled) then
            tri(:, 1) = [cx, cy]
            tri(:, 2) = [cx + radius*cos(a0), cy + radius*sin(a0)]
            tri(:, 3) = [cx + radius*cos(a1), cy + radius*sin(a1)]
            call push_tri(self, tri, col)
         else
            quad(:, 1) = [cx + radius*cos(a0), cy + radius*sin(a0)]
            quad(:, 2) = [cx + radius*cos(a1), cy + radius*sin(a1)]
            quad(:, 3) = [cx + r1*cos(a1), cy + r1*sin(a1)]
            quad(:, 4) = [cx + r1*cos(a0), cy + r1*sin(a0)]
            call push_quad4(self, quad, col)
         end if
      end do
   end subroutine emit_circle

   !> One filled quadrant of a disc, used for rounded corners (no overlap).
   subroutine emit_quarter(self, cx, cy, radius, quad, col)
      class(ui_batch), intent(inout) :: self
      real(wp), intent(in) :: cx, cy, radius
      integer, intent(in) :: quad
      type(ui_color), intent(in) :: col
      real(wp), parameter :: PI = 3.141592653589793_wp
      real(wp) :: a0, a1, tri(2, 3), segf
      integer :: j, nseg

      segf = max(6.0_wp, min(48.0_wp, 1.6_wp*radius + 4.0_wp))
      nseg = int(segf)
      do j = 0, nseg - 1
         ! quadrant 0 = top-left, then clockwise (screen space, y down)
         a0 = PI*(1.0_wp + 0.5_wp*real(quad, wp) + 0.5_wp*real(j, wp)/real(nseg, wp))
         a1 = PI*(1.0_wp + 0.5_wp*real(quad, wp) + 0.5_wp*real(j + 1, wp)/real(nseg, wp))
         tri(:, 1) = [cx, cy]
         tri(:, 2) = [cx + radius*cos(a0), cy + radius*sin(a0)]
         tri(:, 3) = [cx + radius*cos(a1), cy + radius*sin(a1)]
         call push_tri(self, tri, col)
      end do
   end subroutine emit_quarter

   !> Continuous rounded-rect ring: straight runs and corner arcs share their
   !> endpoints exactly, so the stroke has constant width with no gaps.
   subroutine emit_round_ring(self, x, y, w, h, radius, t, col)
      class(ui_batch), intent(inout) :: self
      real(wp), intent(in) :: x, y, w, h, radius, t
      type(ui_color), intent(in) :: col
      real(wp), parameter :: PI = 3.141592653589793_wp
      real(wp) :: r, ti, a0, a1, o0(2), o1(2), i0(2), i1(2), quad(2, 4)
      real(wp) :: cx(4), cy(4), base(4)
      integer :: k, j, nseg

      if (w <= 0.0_wp .or. h <= 0.0_wp) return
      r = min(radius, min(w, h)*0.5_wp)
      ti = min(t, min(w, h)*0.5_wp)
      if (r < 0.5_wp) then
         call push_rect(self, x, y, w, ti, col)
         call push_rect(self, x, y + h - ti, w, ti, col)
         call push_rect(self, x, y + ti, ti, h - 2.0_wp*ti, col)
         call push_rect(self, x + w - ti, y + ti, ti, h - 2.0_wp*ti, col)
         return
      end if

      ! straight runs (they stop exactly where the arcs start)
      call push_rect(self, x + r, y, w - 2.0_wp*r, ti, col)
      call push_rect(self, x + r, y + h - ti, w - 2.0_wp*r, ti, col)
      call push_rect(self, x, y + r, ti, h - 2.0_wp*r, col)
      call push_rect(self, x + w - ti, y + r, ti, h - 2.0_wp*r, col)

      ! corner arcs: TL, TR, BR, BL with point = c + r*(cos, sin), y down
      cx = [x + r, x + w - r, x + w - r, x + r]
      cy = [y + r, y + r, y + h - r, y + h - r]
      base = [PI, 1.5_wp*PI, 0.0_wp, 0.5_wp*PI]
      nseg = max(8, min(96, int(r*3.0_wp)))
      do k = 1, 4
         do j = 0, nseg - 1
            a0 = base(k) + 0.5_wp*PI*real(j, wp)/real(nseg, wp)
            a1 = base(k) + 0.5_wp*PI*real(j + 1, wp)/real(nseg, wp)
            o0 = [cx(k) + r*cos(a0), cy(k) + r*sin(a0)]
            o1 = [cx(k) + r*cos(a1), cy(k) + r*sin(a1)]
            i0 = [cx(k) + (r - ti)*cos(a0), cy(k) + (r - ti)*sin(a0)]
            i1 = [cx(k) + (r - ti)*cos(a1), cy(k) + (r - ti)*sin(a1)]
            quad(:, 1) = o0
            quad(:, 2) = o1
            quad(:, 3) = i1
            quad(:, 4) = i0
            call push_quad4(self, quad, col)
         end do
      end do
   end subroutine emit_round_ring

   !> Rounded rect whose interior is the blurred backdrop tinted by `tint`.
   !> Mode 1 = frosted (Mica), mode 2 = raw backdrop.
   subroutine emit_glass(self, x, y, w, h, radius, tint, mode)
      class(ui_batch), intent(inout) :: self
      real(wp), intent(in) :: x, y, w, h, radius
      type(ui_color), intent(in) :: tint
      integer, intent(in), optional :: mode
      real(wp) :: r, a0, a1, tri(2, 3)
      integer :: k, nseg, md

      md = UI_MODE_GLASS
      if (present(mode)) md = mode
      if (w <= 0.0_wp .or. h <= 0.0_wp) return
      r = min(radius, min(w, h)*0.5_wp)
      if (md == UI_MODE_GLASS) then
         self%glass_rect = [x, y, w, h]
         self%glass_r = r
      end if
      if (r < 0.5_wp) then
         call glass_quad(self, x, y, w, h, tint, md)
         return
      end if

      call glass_quad(self, x, y + r, w, h - 2.0_wp*r, tint, md)
      call glass_quad(self, x + r, y, w - 2.0_wp*r, r, tint, md)
      call glass_quad(self, x + r, y + h - r, w - 2.0_wp*r, r, tint, md)

      nseg = max(8, int(r*2.0_wp))
      do k = 0, 3
         select case (k)
         case (0)
            call glass_fan(self, x + r, y + r, r, 3.141592653589793_wp, &
                           0.5_wp*3.141592653589793_wp, nseg, tint, md)
         case (1)
            call glass_fan(self, x + w - r, y + r, r, 0.5_wp*3.141592653589793_wp, &
                           0.0_wp, nseg, tint, md)
         case (2)
            call glass_fan(self, x + w - r, y + h - r, r, 0.0_wp, &
                           -0.5_wp*3.141592653589793_wp, nseg, tint, md)
         case (3)
            call glass_fan(self, x + r, y + h - r, r, -0.5_wp*3.141592653589793_wp, &
                           -3.141592653589793_wp, nseg, tint, md)
         end select
      end do
      if (.false.) print *, a0, a1, tri(1, 1)
   end subroutine emit_glass

   subroutine glass_quad(self, x, y, w, h, col, md)
      class(ui_batch), intent(inout) :: self
      real(wp), intent(in) :: x, y, w, h
      type(ui_color), intent(in) :: col
      integer, intent(in) :: md
      real(wp) :: cx, cy, hw, hh, rr

      if (w <= 0.0_wp .or. h <= 0.0_wp) return
      ! shape of the whole body this quad belongs to
      cx = self%glass_rect(1) + self%glass_rect(3)*0.5_wp
      cy = self%glass_rect(2) + self%glass_rect(4)*0.5_wp
      hw = self%glass_rect(3)*0.5_wp
      hh = self%glass_rect(4)*0.5_wp
      rr = self%glass_r
      call glass_v(self, x, y, cx, cy, hw, hh, rr, col, md)
      call glass_v(self, x + w, y, cx, cy, hw, hh, rr, col, md)
      call glass_v(self, x + w, y + h, cx, cy, hw, hh, rr, col, md)
      call glass_v(self, x, y, cx, cy, hw, hh, rr, col, md)
      call glass_v(self, x + w, y + h, cx, cy, hw, hh, rr, col, md)
      call glass_v(self, x, y + h, cx, cy, hw, hh, rr, col, md)
   end subroutine glass_quad

   !> One glass vertex carrying its own body description.
   subroutine glass_v(self, px, py, cx, cy, hw, hh, rr, col, md)
      class(ui_batch), intent(inout) :: self
      real(wp), intent(in) :: px, py, cx, cy, hw, hh, rr
      type(ui_color), intent(in) :: col
      integer, intent(in) :: md
      integer :: i

      call push_v(self, px, py, 0.0_wp, 0.0_wp, col, md)
      i = self%count
      self%v(i)%lx = real(px - cx, c_float)
      self%v(i)%ly = real(py - cy, c_float)
      self%v(i)%hw = real(hw, c_float)
      self%v(i)%hh = real(hh, c_float)
      self%v(i)%rad = real(rr, c_float)
   end subroutine glass_v

   !> Circular fan between two angles, used for the rounded corners.
   subroutine glass_fan(self, cx, cy, r, a_start, a_end, nseg, col, md)
      class(ui_batch), intent(inout) :: self
      real(wp), intent(in) :: cx, cy, r, a_start, a_end
      integer, intent(in) :: nseg, md
      type(ui_color), intent(in) :: col
      real(wp) :: a0, a1
      integer :: j

      do j = 0, nseg - 1
         a0 = a_start + (a_end - a_start)*real(j, wp)/real(nseg, wp)
         a1 = a_start + (a_end - a_start)*real(j + 1, wp)/real(nseg, wp)
         call glass_v(self, cx, cy, self%glass_rect(1) + self%glass_rect(3)*0.5_wp, &
                      self%glass_rect(2) + self%glass_rect(4)*0.5_wp, &
                      self%glass_rect(3)*0.5_wp, self%glass_rect(4)*0.5_wp, &
                      self%glass_r, col, md)
         call glass_v(self, cx + r*cos(a0), cy - r*sin(a0), &
                      self%glass_rect(1) + self%glass_rect(3)*0.5_wp, &
                      self%glass_rect(2) + self%glass_rect(4)*0.5_wp, &
                      self%glass_rect(3)*0.5_wp, self%glass_rect(4)*0.5_wp, &
                      self%glass_r, col, md)
         call glass_v(self, cx + r*cos(a1), cy - r*sin(a1), &
                      self%glass_rect(1) + self%glass_rect(3)*0.5_wp, &
                      self%glass_rect(2) + self%glass_rect(4)*0.5_wp, &
                      self%glass_rect(3)*0.5_wp, self%glass_rect(4)*0.5_wp, &
                      self%glass_r, col, md)
      end do
   end subroutine glass_fan

   !> Add one rectangle in UI pixel coordinates to an existing batch.
   !> Requires batch%vw / batch%vh to be set (done by build).
   subroutine batch_rect_px(self, x, y, w, h, col)
      class(ui_batch), intent(inout) :: self
      real(wp), intent(in) :: x, y, w, h
      type(ui_color), intent(in) :: col

      call push_rect(self, x, y, w, h, col)
   end subroutine batch_rect_px

   !> Add one thick line segment in UI pixel coordinates to an existing batch.
   subroutine ui_batch_line_px(self, x0, y0, x1, y1, col, width)
      class(ui_batch), intent(inout) :: self
      real(wp), intent(in) :: x0, y0, x1, y1, width
      type(ui_color), intent(in) :: col

      call emit_line(self, x0, y0, x1, y1, col, width)
   end subroutine ui_batch_line_px

   !> Text, one quad per glyph, sampling the atlas. If a vector text sink is   !> installed the whole string is handed to it instead.
   subroutine emit_text(self, x, y, s, col, scale)
      class(ui_batch), intent(inout) :: self
      real(wp), intent(in) :: x, y, scale
      character(*), intent(in) :: s
      type(ui_color), intent(in) :: col
      integer :: i, idx, col_i, row_i, sc, n
      real(wp) :: x0, y0, u0, u1, v0, v1

      if (associated(g_text_sink)) then
         call g_text_sink(self, x, y, s, col, scale)
         return
      end if

      sc = max(nint(scale), 1)
      n = len_trim(s)

      do i = 1, n
         idx = iachar(s(i:i)) - 32
         if (idx < 0 .or. idx > 94) idx = 31   ! '?' fallback
         col_i = mod(idx, ATLAS_COLS)
         row_i = idx/ATLAS_COLS

         x0 = x + real((i - 1)*UI_FONT_ADVANCE*sc, wp)
         y0 = y
         u0 = real(col_i*CELL, wp)/UI_ATLAS_W
         u1 = real(col_i*CELL + 5, wp)/UI_ATLAS_W
         v0 = real(row_i*CELL, wp)/UI_ATLAS_H
         v1 = real(row_i*CELL + 7, wp)/UI_ATLAS_H

         call push_quad_uv(self, x0, y0, x0 + 5.0_wp*sc, y0 + 7.0_wp*sc, &
                           u0, v0, u1, v1, col)
      end do
   end subroutine emit_text

   !> Convert a whole draw list (or a command range, for split passes) into
   !> one triangle list. Keeping the content pass and the glass pass separate
   !> is what lets glass sample the already rendered scene.
   subroutine batch_build(self, dl, vw, vh, first, last)
      class(ui_batch), intent(inout) :: self
      type(ui_drawlist), intent(in) :: dl
      integer, intent(in) :: vw, vh
      integer, intent(in), optional :: first, last
      integer :: i, i0, i1
      real(wp) :: t

      i0 = 1
      i1 = dl%count
      if (present(first)) i0 = max(1, first)
      if (present(last)) i1 = min(dl%count, last)

      self%count = 0
      self%vw = max(vw, 1)
      self%vh = max(vh, 1)
      call ui_font_init()

      do i = i0, i1
         select case (dl%cmd(i)%kind)
         case (UI_DRAW_RECT)
            call push_rect(self, dl%cmd(i)%a(1), dl%cmd(i)%a(2), &
                           dl%cmd(i)%b(1), dl%cmd(i)%b(2), dl%cmd(i)%fg)
         case (UI_DRAW_RECT_OUTLINE)
            t = max(dl%cmd(i)%thickness, 1.0_wp)
            call push_rect(self, dl%cmd(i)%a(1), dl%cmd(i)%a(2), &
                           dl%cmd(i)%b(1), t, dl%cmd(i)%fg)
            call push_rect(self, dl%cmd(i)%a(1), dl%cmd(i)%a(2) + dl%cmd(i)%b(2) - t, &
                           dl%cmd(i)%b(1), t, dl%cmd(i)%fg)
            call push_rect(self, dl%cmd(i)%a(1), dl%cmd(i)%a(2), &
                           t, dl%cmd(i)%b(2), dl%cmd(i)%fg)
            call push_rect(self, dl%cmd(i)%a(1) + dl%cmd(i)%b(1) - t, dl%cmd(i)%a(2), &
                           t, dl%cmd(i)%b(2), dl%cmd(i)%fg)
         case (UI_DRAW_TEXT)
            call emit_text(self, dl%cmd(i)%a(1), dl%cmd(i)%a(2), dl%cmd(i)%text, &
                           dl%cmd(i)%fg, dl%cmd(i)%scale)
         case (UI_DRAW_LINE)
            call emit_line(self, dl%cmd(i)%a(1), dl%cmd(i)%a(2), &
                           dl%cmd(i)%b(1), dl%cmd(i)%b(2), dl%cmd(i)%fg, &
                           dl%cmd(i)%thickness)
         case (UI_DRAW_CIRCLE)
            call emit_circle(self, dl%cmd(i)%a(1), dl%cmd(i)%a(2), dl%cmd(i)%b(1), &
                             dl%cmd(i)%fg, .true., 0.0_wp)
         case (UI_DRAW_CIRCLE_OUTLINE)
            call emit_circle(self, dl%cmd(i)%a(1), dl%cmd(i)%a(2), dl%cmd(i)%b(1), &
                             dl%cmd(i)%fg, .false., dl%cmd(i)%thickness)
         case (UI_DRAW_RING)
            call emit_round_ring(self, dl%cmd(i)%a(1), dl%cmd(i)%a(2), &
                                 dl%cmd(i)%b(1), dl%cmd(i)%b(2), dl%cmd(i)%scale, &
                                 dl%cmd(i)%thickness, dl%cmd(i)%fg)
         case (UI_DRAW_QUARTER)
            call emit_quarter(self, dl%cmd(i)%a(1), dl%cmd(i)%a(2), &
                              dl%cmd(i)%b(1), int(dl%cmd(i)%scale), dl%cmd(i)%fg)
         case (UI_DRAW_GLASS)
            call emit_glass(self, dl%cmd(i)%a(1), dl%cmd(i)%a(2), dl%cmd(i)%b(1), &
                            dl%cmd(i)%b(2), dl%cmd(i)%thickness, dl%cmd(i)%fg, &
                            int(dl%cmd(i)%scale))
         end select
      end do
   end subroutine batch_build

end module ui_geom
