!> GLFW 3 platform backend.
!>
!> Enabled with -DFUI_HAVE_GLFW and linking against glfw3.
!> Only the GLFW entry points are bound; no other C library is touched.
!> GLFW gives you the window and the Vulkan/GL surface creation helpers, so
!> this single file covers Linux (X11 + Wayland), the BSDs and macOS.
#ifdef FUI_HAVE_GLFW
module ui_plat_glfw
   use, intrinsic :: iso_c_binding   use ui_kinds
   use ui_input
   use ui_plat
   implicit none
   private

   public :: ui_glfw_backend

   integer(c_int), parameter :: GLFW_PRESS = 1
   integer(c_int), parameter :: GLFW_KEY_ESCAPE = 256
   integer(c_int), parameter :: GLFW_KEY_ENTER = 257
   integer(c_int), parameter :: GLFW_KEY_TAB = 258
   integer(c_int), parameter :: GLFW_KEY_BACKSPACE = 259
   integer(c_int), parameter :: GLFW_MOUSE_BUTTON_LEFT = 0
   integer(c_int), parameter :: GLFW_VISIBLE = 2, GLFW_RESIZABLE = 4

   interface
      function glfwInit() bind(c, name='glfwInit') result(ok)
         import :: c_int
         integer(c_int) :: ok
      end function glfwInit

      subroutine glfwTerminate() bind(c, name='glfwTerminate')
      end subroutine glfwTerminate

      function glfwCreateWindow(w, h, title, monitor, share) &
         bind(c, name='glfwCreateWindow') result(win)
         import :: c_int, c_ptr, c_char
         integer(c_int), value :: w, h
         character(kind=c_char), dimension(*) :: title
         type(c_ptr), value :: monitor, share
         type(c_ptr) :: win
      end function glfwCreateWindow

      subroutine glfwDestroyWindow(win) bind(c, name='glfwDestroyWindow')
         import :: c_ptr
         type(c_ptr), value :: win
      end subroutine glfwDestroyWindow

      subroutine glfwMakeContextCurrent(win) bind(c, name='glfwMakeContextCurrent')
         import :: c_ptr
         type(c_ptr), value :: win
      end subroutine glfwMakeContextCurrent

      subroutine glfwSwapBuffers(win) bind(c, name='glfwSwapBuffers')
         import :: c_ptr
         type(c_ptr), value :: win
      end subroutine glfwSwapBuffers

      subroutine glfwSwapInterval(i) bind(c, name='glfwSwapInterval')
         import :: c_int
         integer(c_int), value :: i
      end subroutine glfwSwapInterval

      subroutine glfwPollEvents() bind(c, name='glfwPollEvents')
      end subroutine glfwPollEvents

      subroutine glfwWaitEventsTimeout(t) bind(c, name='glfwWaitEventsTimeout')
         import :: c_double
         real(c_double), value :: t
      end subroutine glfwWaitEventsTimeout

      function glfwWindowShouldClose(win) bind(c, name='glfwWindowShouldClose') result(r)
         import :: c_int, c_ptr
         type(c_ptr), value :: win
         integer(c_int) :: r
      end function glfwWindowShouldClose

      subroutine glfwSetWindowShouldClose(win, v) bind(c, name='glfwSetWindowShouldClose')
         import :: c_int, c_ptr
         type(c_ptr), value :: win
         integer(c_int), value :: v
      end subroutine glfwSetWindowShouldClose

      subroutine glfwGetFramebufferSize(win, w, h) bind(c, name='glfwGetFramebufferSize')
         import :: c_int, c_ptr
         type(c_ptr), value :: win
         integer(c_int) :: w, h
      end subroutine glfwGetFramebufferSize

      subroutine glfwGetWindowSize(win, w, h) bind(c, name='glfwGetWindowSize')
         import :: c_int, c_ptr
         type(c_ptr), value :: win
         integer(c_int) :: w, h
      end subroutine glfwGetWindowSize

      subroutine glfwGetCursorPos(win, x, y) bind(c, name='glfwGetCursorPos')
         import :: c_double, c_ptr
         type(c_ptr), value :: win
         real(c_double) :: x, y
      end subroutine glfwGetCursorPos

      function glfwGetMouseButton(win, b) bind(c, name='glfwGetMouseButton') result(r)
         import :: c_int, c_ptr
         type(c_ptr), value :: win
         integer(c_int), value :: b
         integer(c_int) :: r
      end function glfwGetMouseButton

      function glfwGetKey(win, k) bind(c, name='glfwGetKey') result(r)
         import :: c_int, c_ptr
         type(c_ptr), value :: win
         integer(c_int), value :: k
         integer(c_int) :: r
      end function glfwGetKey

      function glfwGetContentScale(win, x, y) bind(c, name='glfwGetWindowContentScale') result(r)
         import :: c_float, c_ptr
         type(c_ptr), value :: win
         real(c_float) :: x, y
      end function glfwGetContentScale
   end interface

   type, extends(ui_plat_backend) :: ui_glfw_backend
      type(c_ptr) :: win = c_null_ptr
      type(ui_plat_state) :: state
      logical :: initialised = .false.
   contains
      procedure :: create => glfw_create
      procedure :: poll => glfw_poll
      procedure :: shutdown => glfw_shutdown
      procedure :: name => glfw_name
      procedure :: window_ptr => glfw_window_ptr
      procedure :: required_extension => glfw_required_extension
   end type ui_glfw_backend

contains

   !> Create the window. width/height are in logical units.
   subroutine glfw_create(self, width, height, title, vulkan)
      class(ui_glfw_backend), intent(inout) :: self
      integer, intent(in) :: width, height
      character(*), intent(in) :: title
      logical, intent(in), optional :: vulkan
      integer :: ok

      if (present(vulkan)) then
         if (vulkan) then
            ! glfwWindowHint(GLFW_CLIENT_API, GLFW_NO_API)
            call glfwWindowHint(0x00022001_c_int, 0_c_int)
         end if
      end if

      ok = glfwInit()
      if (ok == 0) then
         self%win = c_null_ptr
         return
      end if
      self%initialised = .true.

      self%win = glfwCreateWindow(int(width, c_int), int(height, c_int), &
                                  cstr(title), c_null_ptr, c_null_ptr)
      if (c_associated(self%win)) then
         call glfwMakeContextCurrent(self%win)
         call glfwSwapInterval(1_c_int)
         self%width = width
         self%height = height
      end if
   end subroutine glfw_create

   function glfw_window_ptr(self) result(p)
      class(ui_glfw_backend), intent(in) :: self
      type(c_ptr) :: p

      p = self%win
   end function glfw_window_ptr

   !> Name of the Vulkan instance extension GLFW needs for its surface.
   subroutine glfw_required_extension(self, ext)
      class(ui_glfw_backend), intent(inout) :: self
      character(len=64), intent(out) :: ext
      type(c_ptr) :: names
      character(kind=c_char), pointer :: p(:)
      integer :: i

      names = glfwGetRequiredInstanceExtensions()
      ext = ''
      if (.not. c_associated(names)) return
      call c_f_pointer(names, p, [1024])
      do i = 1, 64
         if (p(i) == c_null_char) exit
         ext(i:i) = p(i)
      end do
   end subroutine glfw_required_extension

   subroutine glfw_poll(self, input, alive)
      class(ui_glfw_backend), intent(inout) :: self
      type(ui_input_state), intent(out) :: input
      logical, intent(out) :: alive
      real(c_double) :: mx, my
      real(c_float) :: sx, sy
      integer(c_int) :: fw, fh, down, ke, kb, kt, kes

      input = ui_input_state()
      alive = .false.

      if (.not. c_associated(self%win)) return

      call glfwPollEvents()
      alive = (glfwWindowShouldClose(self%win) == 0_c_int)

      call glfwGetFramebufferSize(self%win, fw, fh)
      if (fw > 0 .and. fh > 0) then
         self%width = int(fw)
         self%height = int(fh)
      end if

      call glfwGetContentScale(self%win, sx, sy)
      self%scale = max(real(sx, wp), 1.0e-3_wp)

      call glfwGetCursorPos(self%win, mx, my)
      down = glfwGetMouseButton(self%win, GLFW_MOUSE_BUTTON_LEFT)
      ke = glfwGetKey(self%win, GLFW_KEY_ENTER)
      kb = glfwGetKey(self%win, GLFW_KEY_BACKSPACE)
      kt = glfwGetKey(self%win, GLFW_KEY_TAB)
      kes = glfwGetKey(self%win, GLFW_KEY_ESCAPE)

      call self%state%update(real(mx, wp)*self%scale, real(my, wp)*self%scale, &
                             down == GLFW_PRESS, ke == GLFW_PRESS, &
                             kb == GLFW_PRESS, kt == GLFW_PRESS, &
                             kes == GLFW_PRESS, input)
   end subroutine glfw_poll

   subroutine glfw_shutdown(self)
      class(ui_glfw_backend), intent(inout) :: self

      if (c_associated(self%win)) then
         call glfwDestroyWindow(self%win)
         self%win = c_null_ptr
      end if
      if (self%initialised) then
         call glfwTerminate()
         self%initialised = .false.
      end if
   end subroutine glfw_shutdown

   function glfw_name(self) result(s)
      class(ui_glfw_backend), intent(in) :: self
      character(len=:), allocatable :: s

      s = 'glfw'
      if (.false.) print *, self%width
   end function glfw_name

   !> Fortran string -> NUL terminated C string.
   pure function cstr(s) result(c)
      character(*), intent(in) :: s
      character(len_trim(s) + 1, kind=c_char) :: c
      integer :: i

      do i = 1, len_trim(s)
         c(i:i) = s(i:i)
      end do
      c(len_trim(s) + 1:len_trim(s) + 1) = c_null_char
   end function cstr

   interface
      subroutine glfwWindowHint(hint, value) bind(c, name='glfwWindowHint')
         import :: c_int
         integer(c_int), value :: hint, value
      end subroutine glfwWindowHint

      function glfwGetRequiredInstanceExtensions() &
         bind(c, name='glfwGetRequiredInstanceExtensions') result(p)
         import :: c_ptr
         type(c_ptr) :: p
      end function glfwGetRequiredInstanceExtensions
   end interface

end module ui_plat_glfw
#endif
