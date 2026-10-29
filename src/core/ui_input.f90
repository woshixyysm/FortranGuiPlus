!> Input state consumed by the immediate-mode context.
!> The framework never touches the OS: the host application (or a small
!> driver) fills this structure once per frame.
module ui_input
   use ui_kinds
   implicit none
   private

   public :: ui_input_state

   type :: ui_input_state
      !> Mouse position in pixels, window coordinate space.
      real(wp) :: mouse_x = -1.0e6_wp
      real(wp) :: mouse_y = -1.0e6_wp
      !> Mouse delta since the previous frame (used for dragging).
      real(wp) :: mouse_dx = 0.0_wp
      real(wp) :: mouse_dy = 0.0_wp
      !> Button state. pressed/released must be true for ONE frame only.
      logical :: mouse_down = .false.
      logical :: mouse_pressed = .false.
      logical :: mouse_released = .false.
      !> Wheel / scroll delta.
      real(wp) :: wheel = 0.0_wp
      !> Typed characters for this frame (UTF-8/ASCII bytes, unused by MVP widgets).
      character(len=64) :: typed = ''
      !> Discrete keys, true for one frame.
      logical :: key_enter = .false.
      logical :: key_backspace = .false.
      logical :: key_tab = .false.
      logical :: key_escape = .false.
   end type ui_input_state

end module ui_input
