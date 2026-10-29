!> Abstract window / input platform interface.
!>
!> A platform backend owns the OS window and turns whatever the OS reports
!> into a ui_input_state. It knows nothing about rendering, so the same
!> GLFW window can drive the Vulkan, GL, D3D or software backend.
module ui_plat
   use ui_kinds
   use ui_input
   implicit none
   private

   public :: ui_plat_backend, ui_plat_state

   type, abstract :: ui_plat_backend
      integer :: width = 0
      integer :: height = 0
      real(wp) :: scale = 1.0_wp
   contains
      !> Gather one frame of input. `alive` is false once the window closes.
      procedure(iface_poll), deferred :: poll
      procedure(iface_shutdown), deferred :: shutdown
      procedure(iface_name), deferred :: name
   end type ui_plat_backend

   !> Turns a sampled (absolute) device state into edge events.
   !> Shared by every polling backend (GLFW, SDL2, ...).
   type :: ui_plat_state
      real(wp) :: prev_x = 0.0_wp
      real(wp) :: prev_y = 0.0_wp
      logical :: prev_down = .false.
      logical :: prev_enter = .false.
      logical :: prev_back = .false.
      logical :: prev_tab = .false.
      logical :: prev_esc = .false.
      logical :: first = .true.
   contains
      procedure :: update => plat_state_update
   end type ui_plat_state

   abstract interface

      subroutine iface_poll(self, input, alive)
         import :: ui_plat_backend, ui_input_state
         class(ui_plat_backend), intent(inout) :: self
         type(ui_input_state), intent(out) :: input
         logical, intent(out) :: alive
      end subroutine iface_poll

      subroutine iface_shutdown(self)
         import :: ui_plat_backend
         class(ui_plat_backend), intent(inout) :: self
      end subroutine iface_shutdown

      function iface_name(self) result(s)
         import :: ui_plat_backend
         class(ui_plat_backend), intent(in) :: self
         character(len=:), allocatable :: s
      end function iface_name

   end interface

contains

   !> Build a ui_input_state from the sampled device state.
   subroutine plat_state_update(self, x, y, down, enter, back, tab, escape, input)
      class(ui_plat_state), intent(inout) :: self
      real(wp), intent(in) :: x, y
      logical, intent(in) :: down, enter, back, tab, escape
      type(ui_input_state), intent(out) :: input

      input = ui_input_state()

      if (self%first) then
         self%first = .false.
         self%prev_x = x
         self%prev_y = y
      end if

      input%mouse_x = x
      input%mouse_y = y
      input%mouse_dx = x - self%prev_x
      input%mouse_dy = y - self%prev_y
      input%mouse_down = down
      input%mouse_pressed = down .and. .not. self%prev_down
      input%mouse_released = (.not. down) .and. self%prev_down
      input%key_enter = enter .and. .not. self%prev_enter
      input%key_backspace = back .and. .not. self%prev_back
      input%key_tab = tab .and. .not. self%prev_tab
      input%key_escape = escape .and. .not. self%prev_esc

      self%prev_x = x
      self%prev_y = y
      self%prev_down = down
      self%prev_enter = enter
      self%prev_back = back
      self%prev_tab = tab
      self%prev_esc = escape
   end subroutine plat_state_update

end module ui_plat
