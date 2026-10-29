!> Retained list of drawing primitives produced by one UI frame.
!> The list is fully renderer independent: any backend can consume it.
module ui_draw
   use ui_kinds
   implicit none
   private

   public :: ui_drawlist, ui_draw_cmd
   public :: UI_DRAW_RECT, UI_DRAW_RECT_OUTLINE, UI_DRAW_TEXT
   public :: UI_DRAW_LINE, UI_DRAW_CIRCLE, UI_DRAW_CIRCLE_OUTLINE
   public :: UI_DRAW_GLASS
   public :: UI_DRAW_QUARTER
   public :: UI_DRAW_RING
   public :: UI_TEXT_MAX
   public :: ui_set_text_measure, ui_measure_text

   integer, parameter :: UI_DRAW_RECT = 1
   integer, parameter :: UI_DRAW_RECT_OUTLINE = 2
   integer, parameter :: UI_DRAW_TEXT = 3
   integer, parameter :: UI_DRAW_LINE = 4
   integer, parameter :: UI_DRAW_CIRCLE = 5
   integer, parameter :: UI_DRAW_CIRCLE_OUTLINE = 6
   integer, parameter :: UI_DRAW_GLASS = 7
   integer, parameter :: UI_DRAW_QUARTER = 8
   integer, parameter :: UI_DRAW_RING = 9

   !> Max characters kept in a single text command.
   integer, parameter :: UI_TEXT_MAX = 128

   abstract interface
      function ui_measure_iface(s, scale) result(w)
         import :: wp
         character(*), intent(in) :: s
         real(wp), intent(in) :: scale
         real(wp) :: w
      end function ui_measure_iface
   end interface

   procedure(ui_measure_iface), pointer, save :: g_measure => null()

   !> One primitive.
   !>  RECT            : a = origin, b = size
   !>  RECT_OUTLINE    : a = origin, b = size, thickness
   !>  TEXT            : a = baseline origin, text, scale
   !>  LINE            : a = start, b = end, thickness
   !>  CIRCLE          : a = centre, b(1) = radius
   type :: ui_draw_cmd
      integer(ip) :: kind = 0
      real(wp) :: a(2) = 0.0_wp
      real(wp) :: b(2) = 0.0_wp
      real(wp) :: thickness = 1.0_wp
      real(wp) :: scale = 1.0_wp
      type(ui_color) :: fg = ui_color(255_ip, 255_ip, 255_ip, 255_ip)
      character(len=UI_TEXT_MAX) :: text = ''
   end type ui_draw_cmd

   !> Growable command buffer.
   type :: ui_drawlist
      type(ui_draw_cmd), allocatable :: cmd(:)
      integer :: count = 0
      integer :: cap = 0
   contains
      procedure :: clear => dl_clear
      procedure :: rect => dl_rect
      procedure :: rect_outline => dl_rect_outline
      procedure :: round_rect => dl_round_rect
      procedure :: round_rect_outline => dl_round_rect_outline
      procedure :: glow => dl_glow
      procedure :: gradient => dl_gradient
      procedure :: text => dl_text
      procedure :: line => dl_line
      procedure :: circle => dl_circle
      procedure :: circle_outline => dl_circle_outline
      procedure :: glass => dl_glass
      procedure :: backdrop => dl_backdrop
      procedure :: quarter => dl_quarter
   end type ui_drawlist

contains

   !> Frosted / Mica panel: rounded rect filled with the blurred backdrop.
   !> radius goes in `thickness`, tint strength in the colour's alpha,
   !> `mode` 1 = frosted, 2 = raw backdrop.
   subroutine dl_glass(self, x, y, w, h, radius, tint, mode)
      class(ui_drawlist), intent(inout) :: self
      real(wp), intent(in) :: x, y, w, h, radius
      type(ui_color), intent(in) :: tint
      integer, intent(in), optional :: mode
      integer :: i

      if (w <= 0.0_wp .or. h <= 0.0_wp) return
      i = dl_new(self)
      self%cmd(i)%kind = UI_DRAW_GLASS
      self%cmd(i)%a = [x, y]
      self%cmd(i)%b = [w, h]
      self%cmd(i)%fg = tint
      self%cmd(i)%thickness = radius
      if (present(mode)) self%cmd(i)%scale = real(mode, wp)
   end subroutine dl_glass

   !> Optional external text metrics. When installed, ui_text_width asks the
   !> font system instead of using the built-in bitmap advance.
   subroutine ui_set_text_measure(f)
      procedure(ui_measure_iface) :: f

      g_measure => f
   end subroutine ui_set_text_measure

   !> Width in pixels, or a negative value when no hook is installed.
   function ui_measure_text(s, scale) result(w)
      character(*), intent(in) :: s
      real(wp), intent(in) :: scale
      real(wp) :: w

      if (associated(g_measure)) then
         w = g_measure(s, scale)
      else
         w = -1.0_wp
      end if
   end function ui_measure_text

   subroutine dl_clear(self)
      class(ui_drawlist), intent(inout) :: self

      self%count = 0
   end subroutine dl_clear

   !> Reserve one command slot and return its index.
   integer function dl_new(self) result(idx)
      class(ui_drawlist), intent(inout) :: self

      if (self%count + 1 > self%cap) call dl_grow(self, self%count + 1)
      self%count = self%count + 1
      idx = self%count
      self%cmd(idx) = ui_draw_cmd()
   end function dl_new

   subroutine dl_grow(self, need)
      class(ui_drawlist), intent(inout) :: self
      integer, intent(in) :: need
      type(ui_draw_cmd), allocatable :: tmp(:)
      integer :: newcap

      if (need <= self%cap) return
      newcap = max(64, 2*max(self%cap, 1))
      do while (newcap < need)
         newcap = 2*newcap
      end do
      allocate (tmp(newcap))
      if (self%count > 0) tmp(1:self%count) = self%cmd(1:self%count)
      call move_alloc(tmp, self%cmd)
      self%cap = newcap
   end subroutine dl_grow

   subroutine dl_rect(self, x, y, w, h, fg)
      class(ui_drawlist), intent(inout) :: self
      real(wp), intent(in) :: x, y, w, h
      type(ui_color), intent(in) :: fg
      integer :: i

      if (w <= 0.0_wp .or. h <= 0.0_wp) return
      i = dl_new(self)
      self%cmd(i)%kind = UI_DRAW_RECT
      self%cmd(i)%a = [x, y]
      self%cmd(i)%b = [w, h]
      self%cmd(i)%fg = fg
   end subroutine dl_rect

   subroutine dl_rect_outline(self, x, y, w, h, fg, thickness)
      class(ui_drawlist), intent(inout) :: self
      real(wp), intent(in) :: x, y, w, h
      type(ui_color), intent(in) :: fg
      real(wp), intent(in), optional :: thickness
      integer :: i

      if (w <= 0.0_wp .or. h <= 0.0_wp) return
      i = dl_new(self)
      self%cmd(i)%kind = UI_DRAW_RECT_OUTLINE
      self%cmd(i)%a = [x, y]
      self%cmd(i)%b = [w, h]
      self%cmd(i)%fg = fg
      if (present(thickness)) self%cmd(i)%thickness = thickness
   end subroutine dl_rect_outline
   !> Rounded rectangle from four non-overlapping bands plus four corner
   !> discs, so translucent fills do not double-blend at the seams.
   subroutine dl_round_rect(self, x, y, w, h, radius, fg)
      class(ui_drawlist), intent(inout) :: self
      real(wp), intent(in) :: x, y, w, h, radius
      type(ui_color), intent(in) :: fg
      real(wp) :: r

      if (w <= 0.0_wp .or. h <= 0.0_wp) return
      r = min(radius, min(w, h)*0.5_wp)
      if (r <= 0.5_wp) then
         call dl_rect(self, x, y, w, h, fg)
         return
      end if

      ! Order matters for alpha: the pieces below must never overlap.
      call dl_rect(self, x, y + r, w, h - 2.0_wp*r, fg)          ! middle
      call dl_rect(self, x + r, y, w - 2.0_wp*r, r, fg)          ! top strip
      call dl_rect(self, x + r, y + h - r, w - 2.0_wp*r, r, fg)  ! bottom strip
      call dl_quarter(self, x + r, y + r, r, 0, fg)
      call dl_quarter(self, x + w - r, y + r, r, 1, fg)
      call dl_quarter(self, x + w - r, y + h - r, r, 2, fg)
      call dl_quarter(self, x + r, y + h - r, r, 3, fg)
   end subroutine dl_round_rect

   !> Fills an area with the raw (blurred) backdrop - mode 2, no tint.
   subroutine dl_backdrop(self, x, y, w, h)
      class(ui_drawlist), intent(inout) :: self
      real(wp), intent(in) :: x, y, w, h

      call dl_glass(self, x, y, w, h, 0.0_wp, &
                    ui_color(0_ip, 0_ip, 0_ip, 0_ip), 2)
   end subroutine dl_backdrop

   !> One filled quadrant of a disc. a = centre, b(1) = radius,
   !> scale = quadrant (0 = bottom right, increasing clockwise on screen).
   subroutine dl_quarter(self, cx, cy, radius, quad, fg)
      class(ui_drawlist), intent(inout) :: self
      real(wp), intent(in) :: cx, cy, radius
      integer, intent(in) :: quad
      type(ui_color), intent(in) :: fg
      integer :: i

      if (radius <= 0.0_wp) return
      i = dl_new(self)
      self%cmd(i)%kind = UI_DRAW_QUARTER
      self%cmd(i)%a = [cx, cy]
      self%cmd(i)%b(1) = radius
      self%cmd(i)%scale = real(mod(quad, 4), wp)
      self%cmd(i)%fg = fg
   end subroutine dl_quarter

   !> Rounded outline as ONE continuous ring. Drawn as a single primitive so
   !> the stroke has a constant width and there are no gaps or double blends
   !> at the joins between the straight runs and the corner arcs.
   subroutine dl_round_rect_outline(self, x, y, w, h, radius, fg, thickness)
      class(ui_drawlist), intent(inout) :: self
      real(wp), intent(in) :: x, y, w, h, radius
      type(ui_color), intent(in) :: fg
      real(wp), intent(in), optional :: thickness
      integer :: i

      if (w <= 0.0_wp .or. h <= 0.0_wp) return
      i = dl_new(self)
      self%cmd(i)%kind = UI_DRAW_RING
      self%cmd(i)%a = [x, y]
      self%cmd(i)%b = [w, h]
      self%cmd(i)%thickness = 1.0_wp
      if (present(thickness)) self%cmd(i)%thickness = max(0.5_wp, thickness)
      self%cmd(i)%scale = radius
      self%cmd(i)%fg = fg
   end subroutine dl_round_rect_outline


   !> Two-colour vertical gradient, emitted as bands so it needs no shader.
   subroutine dl_gradient(self, x, y, w, h, c0, c1, bands)
      class(ui_drawlist), intent(inout) :: self
      real(wp), intent(in) :: x, y, w, h
      type(ui_color), intent(in) :: c0, c1
      integer, intent(in), optional :: bands
      integer :: n, i
      real(wp) :: t, bh
      type(ui_color) :: c

      if (w <= 0.0_wp .or. h <= 0.0_wp) return
      n = 32
      if (present(bands)) n = max(bands, 1)
      bh = h/real(n, wp)

      do i = 0, n - 1
         t = (real(i, wp) + 0.5_wp)/real(n, wp)
         c%r = nint(real(c0%r, wp) + (real(c1%r, wp) - real(c0%r, wp))*t, ip)
         c%g = nint(real(c0%g, wp) + (real(c1%g, wp) - real(c0%g, wp))*t, ip)
         c%b = nint(real(c0%b, wp) + (real(c1%b, wp) - real(c0%b, wp))*t, ip)
         c%a = nint(real(c0%a, wp) + (real(c1%a, wp) - real(c0%a, wp))*t, ip)
         call dl_rect(self, x, y + real(i, wp)*bh, w, bh + 0.75_wp, c)
      end do
   end subroutine dl_gradient

   !> Glow, matching sakura post/glow.fsh: a Gaussian blur of the shape's
   !> own alpha, tinted with the glow colour and scaled by exposure.
   !>
   !>   sigma    = blur_radius / 2
   !>   alpha(d) = exposure * 0.5 * erfc(d / (sigma * sqrt(2)))
   !>
   !> (0.5*erfc is the blurred edge profile of a step, i.e. exactly what a
   !> separable Gaussian of the silhouette produces.)  Shells are emitted
   !> outermost-first so the accumulated coverage rises toward the shape,
   !> giving the same falloff as the two blur passes.
   subroutine dl_glow(self, x, y, w, h, radius, fg, blur, exposure)
      class(ui_drawlist), intent(inout) :: self
      real(wp), intent(in) :: x, y, w, h, radius
      type(ui_color), intent(in) :: fg
      real(wp), intent(in), optional :: blur, exposure
      real(wp) :: b, sigma, d, a, e, k
      integer :: n, i
      type(ui_color) :: c

      b = 12.0_wp
      if (present(blur)) b = max(blur, 1.0_wp)
      e = real(fg%a, wp)/255.0_wp
      if (present(exposure)) e = e*exposure

      sigma = b*0.5_wp
      n = max(nint(b), 1)

      do i = n, 1, -1
         k = real(i, wp)
         d = k
         a = e*0.5_wp*erfc(d/(sigma*1.4142135623730951_wp))
         if (a <= 0.004_wp) cycle
         c = ui_color(fg%r, fg%g, fg%b, nint(a*255.0_wp, ip))
         call dl_round_rect(self, x - k, y - k, w + 2.0_wp*k, h + 2.0_wp*k, &
                            radius + k, c)
      end do
   end subroutine dl_glow

   subroutine dl_text(self, x, y, s, fg, scale)
      class(ui_drawlist), intent(inout) :: self
      real(wp), intent(in) :: x, y
      character(*), intent(in) :: s
      type(ui_color), intent(in) :: fg
      real(wp), intent(in), optional :: scale
      integer :: i, n

      if (len_trim(s) == 0) return
      i = dl_new(self)
      self%cmd(i)%kind = UI_DRAW_TEXT
      self%cmd(i)%a = [x, y]
      self%cmd(i)%fg = fg
      if (present(scale)) self%cmd(i)%scale = scale
      n = min(len_trim(s), UI_TEXT_MAX)
      self%cmd(i)%text = s(1:n)
   end subroutine dl_text

   subroutine dl_line(self, x0, y0, x1, y1, fg, thickness)
      class(ui_drawlist), intent(inout) :: self
      real(wp), intent(in) :: x0, y0, x1, y1
      type(ui_color), intent(in) :: fg
      real(wp), intent(in), optional :: thickness
      integer :: i

      i = dl_new(self)
      self%cmd(i)%kind = UI_DRAW_LINE
      self%cmd(i)%a = [x0, y0]
      self%cmd(i)%b = [x1, y1]
      self%cmd(i)%fg = fg
      if (present(thickness)) self%cmd(i)%thickness = thickness
   end subroutine dl_line

   subroutine dl_circle(self, cx, cy, radius, fg)
      class(ui_drawlist), intent(inout) :: self
      real(wp), intent(in) :: cx, cy, radius
      type(ui_color), intent(in) :: fg
      integer :: i

      i = dl_new(self)
      self%cmd(i)%kind = UI_DRAW_CIRCLE
      self%cmd(i)%a = [cx, cy]
      self%cmd(i)%b = [radius, 0.0_wp]
      self%cmd(i)%fg = fg
   end subroutine dl_circle

   subroutine dl_circle_outline(self, cx, cy, radius, fg, thickness)
      class(ui_drawlist), intent(inout) :: self
      real(wp), intent(in) :: cx, cy, radius
      type(ui_color), intent(in) :: fg
      real(wp), intent(in), optional :: thickness
      integer :: i

      i = dl_new(self)
      self%cmd(i)%kind = UI_DRAW_CIRCLE_OUTLINE
      self%cmd(i)%a = [cx, cy]
      self%cmd(i)%b = [radius, 0.0_wp]
      self%cmd(i)%fg = fg
      if (present(thickness)) self%cmd(i)%thickness = thickness
   end subroutine dl_circle_outline

end module ui_draw
