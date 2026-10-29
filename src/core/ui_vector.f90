!> Vector path: move/line/quadratic/cubic segments, flattening and filling.
!>
!> This is the shared substrate for TrueType glyph outlines (quadratic) and
!> SVG path data (quadratic + cubic). Curves are flattened adaptively against
!> a tolerance in pixels and filled with the even-odd rule by emitting
!> horizontal spans into a ui_batch, so they render through the same GPU
!> pipeline as everything else.
module ui_vector
   use ui_kinds
   use ui_geom
   implicit none
   private

   public :: ui_path, ui_path_tolerance

   integer(ip), parameter :: OP_MOVE = 1
   integer(ip), parameter :: OP_LINE = 2
   integer(ip), parameter :: OP_QUAD = 3
   integer(ip), parameter :: OP_CUBIC = 4
   integer(ip), parameter :: OP_CLOSE = 5

   !> Default flattening tolerance in pixels.
   real(wp), parameter :: ui_path_tolerance = 0.15_wp

   type :: ui_path
      integer(ip), allocatable :: op(:)
      real(wp), allocatable :: c(:, :)     ! 6 x n control points
      integer :: n = 0
      integer :: cap = 0
   contains
      procedure :: reset => path_reset
      procedure :: move_to => path_move
      procedure :: line_to => path_line
      procedure :: quad_to => path_quad
      procedure :: cubic_to => path_cubic
      procedure :: close => path_close
      procedure :: bounds => path_bounds
      procedure :: flatten => path_flatten
      procedure :: fill => path_fill
      procedure :: stroke => path_stroke
      procedure :: transform => path_transform
      procedure :: count => path_count
   end type ui_path

contains

   !------------------------------------------------------------------
   ! construction
   !------------------------------------------------------------------

   subroutine path_reset(self)
      class(ui_path), intent(inout) :: self

      self%n = 0
   end subroutine path_reset

   pure integer function path_count(self) result(n)
      class(ui_path), intent(in) :: self

      n = self%n
   end function path_count

   subroutine path_grow(self, need)
      class(ui_path), intent(inout) :: self
      integer, intent(in) :: need
      integer(ip), allocatable :: opt(:)
      real(wp), allocatable :: ct(:, :)
      integer :: newcap

      if (need <= self%cap) return
      newcap = max(64, 2*max(self%cap, 1))
      do while (newcap < need)
         newcap = 2*newcap
      end do
      allocate (opt(newcap), ct(6, newcap))
      if (self%n > 0) then
         opt(1:self%n) = self%op(1:self%n)
         ct(:, 1:self%n) = self%c(:, 1:self%n)
      end if
      opt(newcap) = 0_ip
      call move_alloc(opt, self%op)
      call move_alloc(ct, self%c)
      self%cap = newcap
   end subroutine path_grow

   subroutine path_push(self, op, p1, p2, p3)
      class(ui_path), intent(inout) :: self
      integer(ip), intent(in) :: op
      real(wp), intent(in), optional :: p1(2), p2(2), p3(2)

      if (self%n + 1 > self%cap) call path_grow(self, self%n + 1)
      self%n = self%n + 1
      self%op(self%n) = op
      self%c(:, self%n) = 0.0_wp
      if (present(p1)) self%c(1:2, self%n) = p1
      if (present(p2)) self%c(3:4, self%n) = p2
      if (present(p3)) self%c(5:6, self%n) = p3
   end subroutine path_push

   subroutine path_move(self, x, y)
      class(ui_path), intent(inout) :: self
      real(wp), intent(in) :: x, y

      call path_push(self, OP_MOVE, [x, y])
   end subroutine path_move

   subroutine path_line(self, x, y)
      class(ui_path), intent(inout) :: self
      real(wp), intent(in) :: x, y

      call path_push(self, OP_LINE, [x, y])
   end subroutine path_line

   subroutine path_quad(self, cx, cy, x, y)
      class(ui_path), intent(inout) :: self
      real(wp), intent(in) :: cx, cy, x, y

      call path_push(self, OP_QUAD, [cx, cy], [x, y])
   end subroutine path_quad

   subroutine path_cubic(self, c1x, c1y, c2x, c2y, x, y)
      class(ui_path), intent(inout) :: self
      real(wp), intent(in) :: c1x, c1y, c2x, c2y, x, y

      call path_push(self, OP_CUBIC, [c1x, c1y], [c2x, c2y], [x, y])
   end subroutine path_cubic

   subroutine path_close(self)
      class(ui_path), intent(inout) :: self

      call path_push(self, OP_CLOSE)
   end subroutine path_close

   !------------------------------------------------------------------
   ! flattening
   !------------------------------------------------------------------

   !> Number of segments needed for a curve with the given control polygon
   !> length so that the sagitta stays below tol.
   pure integer function segments_for(plen, tol) result(n)
      real(wp), intent(in) :: plen, tol

      n = int(sqrt(max(plen/max(tol, 1.0e-4_wp), 1.0_wp)))*3
      n = min(max(n, 2), 96)
   end function segments_for

   !> Flatten to polylines. pts is (2,npts); starts(1..ncont) is the first
   !> point index of each contour.
   subroutine path_flatten(self, tol, pts, starts, npts, ncont)
      class(ui_path), intent(in) :: self
      real(wp), intent(in) :: tol
      real(wp), allocatable, intent(out) :: pts(:, :)
      integer, allocatable, intent(out) :: starts(:)
      integer, intent(out) :: npts, ncont
      real(wp) :: cur(2), sp(2), p0(2), p1(2), p2(2), p3(2), q(2), t, mt
      integer :: i, k, nseg, cap

      cap = max(64, self%n*24)
      allocate (pts(2, cap))
      allocate (starts(max(self%n, 1)))
      npts = 0
      ncont = 0
      cur = [0.0_wp, 0.0_wp]
      sp = cur

      do i = 1, self%n
         select case (self%op(i))
         case (OP_MOVE)
            cur = self%c(1:2, i)
            sp = cur
            ncont = ncont + 1
            starts(ncont) = npts + 1
            call add_point(cur)
         case (OP_LINE)
            cur = self%c(1:2, i)
            call add_point(cur)
         case (OP_QUAD)
            p0 = cur
            p1 = self%c(1:2, i)
            p2 = self%c(3:4, i)
            nseg = segments_for(norm2(p1 - p0) + norm2(p2 - p1), tol)
            do k = 1, nseg
               t = real(k, wp)/real(nseg, wp)
               mt = 1.0_wp - t
               q = mt*mt*p0 + 2.0_wp*mt*t*p1 + t*t*p2
               call add_point(q)
            end do
            cur = p2
         case (OP_CUBIC)
            p0 = cur
            p1 = self%c(1:2, i)
            p2 = self%c(3:4, i)
            p3 = self%c(5:6, i)
            nseg = segments_for(norm2(p1 - p0) + norm2(p2 - p1) + &
                                norm2(p3 - p2), tol)
            do k = 1, nseg
               t = real(k, wp)/real(nseg, wp)
               mt = 1.0_wp - t
               q = mt*mt*mt*p0 + 3.0_wp*mt*mt*t*p1 + &
                   3.0_wp*mt*t*t*p2 + t*t*t*p3
               call add_point(q)
            end do
            cur = p3
         case (OP_CLOSE)
            cur = sp
         end select
      end do

      if (npts > 0) then
         pts(:, npts + 1:cap) = 0.0_wp
      end if

   contains

      !> Append one polyline point, growing the buffer when needed.
      subroutine add_point(p)
         real(wp), intent(in) :: p(2)
         real(wp), allocatable :: tmp(:, :)

         if (npts + 1 > size(pts, 2)) then
            allocate (tmp(2, 2*size(pts, 2)))
            tmp(:, 1:npts) = pts(:, 1:npts)
            call move_alloc(tmp, pts)
         end if
         npts = npts + 1
         pts(:, npts) = p
      end subroutine add_point

   end subroutine path_flatten

   !> Scale by s and translate by (tx,ty), in place. Used to place an SVG
   !> viewBox (typically 24x24) anywhere on screen.
   subroutine path_transform(self, s, tx, ty)
      class(ui_path), intent(inout) :: self
      real(wp), intent(in) :: s, tx, ty
      integer :: i, k

      do i = 1, self%n
         do k = 1, 3
            self%c(2*k - 1, i) = self%c(2*k - 1, i)*s + tx
            self%c(2*k, i) = self%c(2*k, i)*s + ty
         end do
      end do
   end subroutine path_transform

   !> Axis aligned bounds of the flattened path.
   subroutine path_bounds(self, tol, x0, y0, x1, y1)
      class(ui_path), intent(in) :: self
      real(wp), intent(in) :: tol
      real(wp), intent(out) :: x0, y0, x1, y1
      real(wp), allocatable :: pts(:, :)
      integer, allocatable :: starts(:)
      integer :: npts, ncont

      call path_flatten(self, tol, pts, starts, npts, ncont)
      x0 = 0.0_wp; y0 = 0.0_wp; x1 = 0.0_wp; y1 = 0.0_wp
      if (npts < 1) return
      x0 = minval(pts(1, 1:npts)); x1 = maxval(pts(1, 1:npts))
      y0 = minval(pts(2, 1:npts)); y1 = maxval(pts(2, 1:npts))
   end subroutine path_bounds

   !> Fill using the NON-ZERO winding rule (what TrueType and SVG specify).
   !> Each scanline crossing carries its edge direction; a pixel is inside
   !> while the accumulated winding is non-zero, so overlapping contours of
   !> a hinted glyph stay solid instead of punching holes.
   subroutine path_fill(self, batch, col, tol)
      class(ui_path), intent(in) :: self
      class(ui_batch), intent(inout) :: batch
      type(ui_color), intent(in) :: col
      real(wp), intent(in), optional :: tol

      real(wp), allocatable :: pts(:, :)
      integer, allocatable :: starts(:)
      real(wp), allocatable :: xs(:)
      integer, allocatable :: wn(:)
      real(wp) :: tt, y, xa, xb
      integer :: npts, ncont, j, j0, j1, k, nc, a, b, c2, nxt, w

      tt = ui_path_tolerance
      if (present(tol)) tt = tol

      call path_flatten(self, tt, pts, starts, npts, ncont)
      if (npts < 3 .or. ncont < 1) return

      j0 = ceiling(minval(pts(2, 1:npts)))
      j1 = floor(maxval(pts(2, 1:npts)))

      allocate (xs(npts), wn(npts))

      do j = j0, j1
         y = real(j, wp) - 0.5_wp
         nc = 0
         do k = 1, ncont
            a = starts(k)
            if (k < ncont) then
               b = starts(k + 1) - 1
            else
               b = npts
            end if
            if (b - a < 2) cycle
            do c2 = a, b
               nxt = c2 + 1
               if (nxt > b) nxt = a
               call edge_cross(pts(1, c2), pts(2, c2), pts(1, nxt), pts(2, nxt), &
                               y, nc, xs, wn)
            end do
         end do

         if (nc >= 2) then
            call sort_crossings(xs, wn, nc)
            w = 0
            k = 1
            do while (k < nc)
               w = w + wn(k)
               if (w /= 0) then
                  xa = xs(k)
                  xb = xs(k + 1)
                  if (xb > xa) then
                     call batch%rect_px(xa, real(j - 1, wp), xb - xa, 1.0_wp, col)
                  end if
               end if
               k = k + 1
            end do
         end if
      end do
   end subroutine path_fill

   !> Record a scanline crossing together with the edge direction.
   subroutine edge_cross(x0, y0, x1, y1, y, nc, xs, wn)
      real(wp), intent(in) :: x0, y0, x1, y1, y
      integer, intent(inout) :: nc
      real(wp), intent(inout) :: xs(:)
      integer, intent(inout) :: wn(:)
      real(wp) :: t

      if ((y0 <= y) .eqv. (y1 <= y)) return
      t = (y - y0)/(y1 - y0)
      nc = nc + 1
      if (nc > size(xs)) return
      xs(nc) = x0 + t*(x1 - x0)
      if (y1 > y0) then
         wn(nc) = 1
      else
         wn(nc) = -1
      end if
   end subroutine edge_cross

   !> Insertion sort the crossings by x, carrying the winding with them.
   subroutine sort_crossings(v, wn, n)
      real(wp), intent(inout) :: v(:)
      integer, intent(inout) :: wn(:)
      integer, intent(in) :: n
      integer :: i, j, tw
      real(wp) :: tmp

      do i = 2, n
         tmp = v(i)
         tw = wn(i)
         j = i - 1
         do while (j >= 1)
            if (v(j) <= tmp) exit
            v(j + 1) = v(j)
            wn(j + 1) = wn(j)
            j = j - 1
         end do
         v(j + 1) = tmp
         wn(j + 1) = tw
      end do
   end subroutine sort_crossings

   !> Stroke every contour with a thick line.
   subroutine path_stroke(self, batch, col, width, tol)
      class(ui_path), intent(in) :: self
      class(ui_batch), intent(inout) :: batch
      type(ui_color), intent(in) :: col
      real(wp), intent(in) :: width
      real(wp), intent(in), optional :: tol

      real(wp), allocatable :: pts(:, :)
      integer, allocatable :: starts(:)
      real(wp) :: tt
      integer :: npts, ncont, k, a, b, i, nxt

      tt = ui_path_tolerance
      if (present(tol)) tt = tol

      call path_flatten(self, tt, pts, starts, npts, ncont)
      if (npts < 2) return

      do k = 1, ncont
         a = starts(k)
         if (k < ncont) then
            b = starts(k + 1) - 1
         else
            b = npts
         end if
         do i = a, b - 1
            call ui_batch_line_px(batch, pts(1, i), pts(2, i), pts(1, i + 1), &
                                  pts(2, i + 1), col, width)
         end do
      end do
   end subroutine path_stroke

end module ui_vector
