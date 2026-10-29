!> Basic value types shared by the whole framework.
!> Pure Fortran 2008: no C interoperability, no platform headers.
module ui_kinds
   use, intrinsic :: iso_fortran_env, only: real64, int32, int64, int8
   implicit none
   private

   public :: wp, ip, byte, i64, int8
   public :: ui_vec2, ui_rect, ui_color
   public :: ui_clamp, ui_lerp

   !> Working precision used for all geometry.
   integer, parameter :: wp = real64
   !> Integer kind used for colours / ids.
   integer, parameter :: ip = int32
   !> Byte kind used for 8 bit channel values.
   integer, parameter :: byte = int8
   !> Wide kind used internally for hashing without overflow.
   integer, parameter :: i64 = int64

   !> 2D point / vector.
   type :: ui_vec2
      real(wp) :: x = 0.0_wp
      real(wp) :: y = 0.0_wp
   end type ui_vec2

   !> Axis aligned rectangle, origin at top-left, y grows downwards.
   type :: ui_rect
      real(wp) :: x = 0.0_wp
      real(wp) :: y = 0.0_wp
      real(wp) :: w = 0.0_wp
      real(wp) :: h = 0.0_wp
   contains
      procedure :: contains => rect_contains
      procedure :: center_x => rect_center_x
      procedure :: center_y => rect_center_y
      procedure :: right => rect_right
      procedure :: bottom => rect_bottom
   end type ui_rect

   !> RGBA colour, channels in 0..255.
   type :: ui_color
      integer(ip) :: r = 0
      integer(ip) :: g = 0
      integer(ip) :: b = 0
      integer(ip) :: a = 255
   end type ui_color

contains

   pure logical function rect_contains(self, px, py) result(inside)
      class(ui_rect), intent(in) :: self
      real(wp), intent(in) :: px, py

      inside = (px >= self%x) .and. (px < self%x + self%w) &
               .and. (py >= self%y) .and. (py < self%y + self%h)
   end function rect_contains

   pure real(wp) function rect_center_x(self) result(cx)
      class(ui_rect), intent(in) :: self
      cx = self%x + 0.5_wp*self%w
   end function rect_center_x

   pure real(wp) function rect_center_y(self) result(cy)
      class(ui_rect), intent(in) :: self
      cy = self%y + 0.5_wp*self%h
   end function rect_center_y

   pure real(wp) function rect_right(self) result(r)
      class(ui_rect), intent(in) :: self
      r = self%x + self%w
   end function rect_right

   pure real(wp) function rect_bottom(self) result(b)
      class(ui_rect), intent(in) :: self
      b = self%y + self%h
   end function rect_bottom

   !> Clamp a value into [lo, hi].
   pure real(wp) function ui_clamp(v, lo, hi) result(out)
      real(wp), intent(in) :: v, lo, hi

      out = min(max(v, lo), hi)
   end function ui_clamp

   !> Linear interpolation.
   pure real(wp) function ui_lerp(a, b, t) result(out)
      real(wp), intent(in) :: a, b, t

      out = a + (b - a)*t
   end function ui_lerp

end module ui_kinds
