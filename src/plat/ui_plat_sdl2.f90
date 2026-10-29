!> SDL2 platform backend: window, input and an OpenGL context.
!>
!> Only SDL2 is bound (explicitly allowed). Borderless windows are supported
!> because the framework draws its own chrome.
module ui_plat_sdl2
   use, intrinsic :: iso_c_binding
   use ui_kinds
   use ui_input
   use ui_plat
   implicit none
   private

   public :: ui_sdl2_backend
   public :: UI_MODE_OVERLAY, UI_MODE_WINDOW, UI_MODE_CSD

   !> Window modes.
   !>  OVERLAY - borderless, always on top, fixed size  (ImGui style)
   !>  WINDOW  - OS frame, resizable, movable            (normal app window)
   !>  CSD     - borderless + resizable, we draw the frame
   !>            (Wayland xdg_toplevel client side decorations)
   integer, parameter :: UI_MODE_OVERLAY = 0
   integer, parameter :: UI_MODE_WINDOW = 1
   integer, parameter :: UI_MODE_CSD = 2

   integer(c_int), parameter :: SDL_INIT_VIDEO = 32
   integer(c_int), parameter :: SDL_WINDOW_SHOWN = 4
   integer(c_int), parameter :: SDL_WINDOW_OPENGL = 2
   integer(c_int), parameter :: SDL_WINDOW_BORDERLESS = 16
   integer(c_int), parameter :: SDL_WINDOW_RESIZABLE = 32
   integer(c_int), parameter :: SDL_WINDOW_ALLOW_HIGHDPI = 8192
   integer(c_int), parameter :: SDL_GL_MULTISAMPLEBUFFERS = 10
   integer(c_int), parameter :: SDL_GL_MULTISAMPLESAMPLES = 11
   integer(c_int), parameter :: SDL_GL_CONTEXT_MAJOR = 17
   integer(c_int), parameter :: SDL_GL_CONTEXT_MINOR = 18
   integer(c_int), parameter :: SDL_GL_CONTEXT_PROFILE_MASK = 21
   integer(c_int), parameter :: SDL_GL_CONTEXT_PROFILE_CORE = 1
   integer(c_int), parameter :: SDL_GL_DOUBLEBUFFER = 5
   integer(c_int), parameter :: SDL_QUIT_EVENT = 256

   interface
      function SDL_Init(flags) bind(c, name='SDL_Init') result(r)
         import :: c_int
         integer(c_int), value :: flags
         integer(c_int) :: r
      end function SDL_Init

      subroutine SDL_Quit() bind(c, name='SDL_Quit')
      end subroutine SDL_Quit

      function SDL_CreateWindow(title, x, y, w, h, flags) &
         bind(c, name='SDL_CreateWindow') result(p)
         import :: c_int, c_ptr, c_char
         character(kind=c_char) :: title(*)
         integer(c_int), value :: x, y, w, h, flags
         type(c_ptr) :: p
      end function SDL_CreateWindow

      subroutine SDL_DestroyWindow(win) bind(c, name='SDL_DestroyWindow')
         import :: c_ptr
         type(c_ptr), value :: win
      end subroutine SDL_DestroyWindow

      subroutine SDL_SetWindowTitle(win, title) bind(c, name='SDL_SetWindowTitle')
         import :: c_ptr, c_char
         type(c_ptr), value :: win
         character(kind=c_char) :: title(*)
      end subroutine SDL_SetWindowTitle

      subroutine SDL_GetWindowSize(win, w, h) bind(c, name='SDL_GetWindowSize')
         import :: c_ptr, c_int
         type(c_ptr), value :: win
         integer(c_int) :: w, h
      end subroutine SDL_GetWindowSize

      function SDL_GL_CreateContext(win) bind(c, name='SDL_GL_CreateContext') result(p)
         import :: c_ptr
         type(c_ptr), value :: win
         type(c_ptr) :: p
      end function SDL_GL_CreateContext

      subroutine SDL_GL_DeleteContext(ctx) bind(c, name='SDL_GL_DeleteContext')
         import :: c_ptr
         type(c_ptr), value :: ctx
      end subroutine SDL_GL_DeleteContext

      subroutine SDL_GL_SwapWindow(win) bind(c, name='SDL_GL_SwapWindow')
         import :: c_ptr
         type(c_ptr), value :: win
      end subroutine SDL_GL_SwapWindow

      function SDL_GL_SetAttribute(attr, val) bind(c, name='SDL_GL_SetAttribute') result(r)
         import :: c_int
         integer(c_int), value :: attr, val
         integer(c_int) :: r
      end function SDL_GL_SetAttribute

      function SDL_GL_SetSwapInterval(i) bind(c, name='SDL_GL_SetSwapInterval') result(r)
         import :: c_int
         integer(c_int), value :: i
         integer(c_int) :: r
      end function SDL_GL_SetSwapInterval

      function SDL_GL_GetDrawableSize(win, w, h) bind(c, name='SDL_GL_GetDrawableSize') result(r)
         import :: c_ptr, c_int
         type(c_ptr), value :: win
         integer(c_int) :: w, h
         integer(c_int) :: r
      end function SDL_GL_GetDrawableSize

      function SDL_GL_GetProcAddress(name) bind(c, name='SDL_GL_GetProcAddress') result(p)
         import :: c_ptr, c_char
         character(kind=c_char) :: name(*)
         type(c_ptr) :: p
      end function SDL_GL_GetProcAddress

      function SDL_GetMouseState(x, y) bind(c, name='SDL_GetMouseState') result(m)
         import :: c_int
         integer(c_int) :: x, y
         integer(c_int) :: m
      end function SDL_GetMouseState

      function SDL_PollEvent(ev) bind(c, name='SDL_PollEvent') result(r)
         import :: c_int, c_ptr
         type(c_ptr), value :: ev
         integer(c_int) :: r
      end function SDL_PollEvent

      function SDL_GetTicks() bind(c, name='SDL_GetTicks') result(t)
         import :: c_int
         integer(c_int) :: t
      end function SDL_GetTicks

      function SDL_GetError() bind(c, name='SDL_GetError') result(p)
         import :: c_ptr
         type(c_ptr) :: p
      end function SDL_GetError

      subroutine SDL_RaiseWindow(win) bind(c, name='SDL_RaiseWindow')
         import :: c_ptr
         type(c_ptr), value :: win
      end subroutine SDL_RaiseWindow

      subroutine SDL_SetWindowPosition(win, x, y) bind(c, name='SDL_SetWindowPosition')
         import :: c_ptr, c_int
         type(c_ptr), value :: win
         integer(c_int), value :: x, y
      end subroutine SDL_SetWindowPosition

      subroutine SDL_SetWindowAlwaysOnTop(win, on) bind(c, name='SDL_SetWindowAlwaysOnTop')
         import :: c_ptr, c_int
         type(c_ptr), value :: win
         integer(c_int), value :: on
      end subroutine SDL_SetWindowAlwaysOnTop

      function SDL_SetHint(name, value) bind(c, name='SDL_SetHint') result(r)
         import :: c_char, c_int
         character(kind=c_char) :: name(*), value(*)
         integer(c_int) :: r
      end function SDL_SetHint
   end interface

   type, extends(ui_plat_backend) :: ui_sdl2_backend
      type(c_ptr) :: win = c_null_ptr
      type(c_ptr) :: gl = c_null_ptr
      type(ui_plat_state) :: state
      logical :: running = .false.
      logical :: borderless = .false.
      integer :: mode = UI_MODE_OVERLAY
      logical :: closed = .false.
      integer :: last_ticks = 0
      real(wp) :: dt = 1.0_wp/60.0_wp
   contains
      procedure :: create => sdl_create
      procedure :: poll => sdl_poll
      procedure :: shutdown => sdl_shutdown
      procedure :: name => sdl_name
      procedure :: swap => sdl_swap
      procedure :: proc => sdl_proc
      procedure :: window_ptr => sdl_window_ptr
      procedure :: request_close => sdl_request_close
   end type ui_sdl2_backend

contains

   !> Create the window and a core-profile GL context.
   function sdl_create(self, width, height, title, borderless, mode, driver) result(ok)
      class(ui_sdl2_backend), intent(inout) :: self
      integer, intent(in) :: width, height
      character(*), intent(in) :: title
      logical, intent(in), optional :: borderless
      integer, intent(in), optional :: mode
      character(*), intent(in), optional :: driver
      logical :: ok
      integer(c_int) :: flags, r

      ok = .false.
      self%mode = UI_MODE_OVERLAY
      if (present(mode)) self%mode = mode
      self%borderless = (self%mode == UI_MODE_OVERLAY) .or. (self%mode == UI_MODE_CSD)
      if (present(borderless)) then
         if (borderless) self%borderless = .true.
      end if

      ! Wayland / x11 selection must be set before SDL_Init
      if (present(driver)) then
         if (len_trim(driver) > 0) r = SDL_SetHint(cstr('SDL_VIDEODRIVER'), cstr(driver))
      end if

      r = SDL_Init(SDL_INIT_VIDEO)
      if (r /= 0) return

      r = SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR, 3_c_int)
      r = SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR, 3_c_int)
      r = SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, &
                              SDL_GL_CONTEXT_PROFILE_CORE)
      r = SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1_c_int)
      ! 4x MSAA: every rounded corner and glyph edge in the batch is geometry,
      ! so multisampling is what makes the edges clean.
      r = SDL_GL_SetAttribute(SDL_GL_MULTISAMPLEBUFFERS, 1_c_int)
      r = SDL_GL_SetAttribute(SDL_GL_MULTISAMPLESAMPLES, 4_c_int)

      flags = SDL_WINDOW_OPENGL + SDL_WINDOW_SHOWN + SDL_WINDOW_ALLOW_HIGHDPI
      select case (self%mode)
      case (UI_MODE_OVERLAY)
         flags = flags + SDL_WINDOW_BORDERLESS
      case (UI_MODE_WINDOW)
         flags = flags + SDL_WINDOW_RESIZABLE
      case (UI_MODE_CSD)
         flags = flags + SDL_WINDOW_BORDERLESS + SDL_WINDOW_RESIZABLE
      end select

      self%win = SDL_CreateWindow(cstr(title), 80_c_int, 60_c_int, &
                                  int(width, c_int), int(height, c_int), flags)
      if (.not. c_associated(self%win)) return

      self%gl = SDL_GL_CreateContext(self%win)
      if (.not. c_associated(self%gl)) return

      r = SDL_GL_SetSwapInterval(1_c_int)
      call SDL_SetWindowPosition(self%win, 80_c_int, 60_c_int)
      if (self%mode == UI_MODE_OVERLAY) then
         call SDL_SetWindowAlwaysOnTop(self%win, 1_c_int)
      else
         call SDL_SetWindowAlwaysOnTop(self%win, 0_c_int)
      end if
      call SDL_RaiseWindow(self%win)
      self%running = .true.
      self%closed = .false.
      self%width = width
      self%height = height
      self%last_ticks = int(SDL_GetTicks())
      ok = .true.
   end function sdl_create

   function sdl_window_ptr(self) result(p)
      class(ui_sdl2_backend), intent(in) :: self
      type(c_ptr) :: p

      p = self%win
   end function sdl_window_ptr

   !> Resolve a GL entry point through SDL.
   function sdl_proc(self, symbol) result(p)
      class(ui_sdl2_backend), intent(in) :: self
      character(*), intent(in) :: symbol
      type(c_ptr) :: p

      p = SDL_GL_GetProcAddress(cstr(symbol))
      if (.false.) print *, self%width
   end function sdl_proc

   subroutine sdl_swap(self)
      class(ui_sdl2_backend), intent(inout) :: self

      if (c_associated(self%win)) call SDL_GL_SwapWindow(self%win)
   end subroutine sdl_swap

   subroutine sdl_request_close(self)
      class(ui_sdl2_backend), intent(inout) :: self

      self%closed = .true.
   end subroutine sdl_request_close

   !> Pump events and sample the mouse.
   subroutine sdl_poll(self, input, alive)
      class(ui_sdl2_backend), intent(inout) :: self
      type(ui_input_state), intent(out) :: input
      logical, intent(out) :: alive
      character(kind=c_char), target :: ev(56)
      integer(c_int) :: r, mx, my, mask, dw, dh, ww, hh, now
      integer :: etype
      real(wp) :: scale

      input = ui_input_state()
      alive = .false.
      if (.not. c_associated(self%win)) return

      do
         r = SDL_PollEvent(c_loc(ev))
         if (r == 0) exit
         etype = iand(int(iachar(ev(1))) + 256*int(iachar(ev(2))) + &
                      65536*int(iachar(ev(3))) + 16777216*int(iachar(ev(4))), 65535)
         if (etype == SDL_QUIT_EVENT) self%closed = .true.
      end do

      alive = .not. self%closed
      if (.not. alive) return

      call SDL_GetWindowSize(self%win, ww, hh)
      r = SDL_GL_GetDrawableSize(self%win, dw, dh)
      if (dw > 0 .and. dh > 0) then
         self%width = int(dw)
         self%height = int(dh)
      else
         self%width = int(ww)
         self%height = int(hh)
      end if
      scale = real(self%width, wp)/max(real(ww, wp), 1.0_wp)
      self%scale = max(scale, 1.0_wp)

      mask = SDL_GetMouseState(mx, my)
      call self%state%update(real(mx, wp)*self%scale, real(my, wp)*self%scale, &
                             iand(mask, 1) /= 0, .false., .false., .false., &
                             .false., input)

      now = int(SDL_GetTicks())
      if (self%last_ticks > 0) then
         self%dt = max(real(now - self%last_ticks, wp)/1000.0_wp, 1.0e-3_wp)
      end if
      self%last_ticks = now
   end subroutine sdl_poll

   subroutine sdl_shutdown(self)
      class(ui_sdl2_backend), intent(inout) :: self

      if (c_associated(self%gl)) then
         call SDL_GL_DeleteContext(self%gl)
         self%gl = c_null_ptr
      end if
      if (c_associated(self%win)) then
         call SDL_DestroyWindow(self%win)
         self%win = c_null_ptr
      end if
      if (self%running) then
         call SDL_Quit()
         self%running = .false.
      end if
   end subroutine sdl_shutdown

   function sdl_name(self) result(s)
      class(ui_sdl2_backend), intent(in) :: self
      character(len=:), allocatable :: s

      s = 'sdl2'
      if (.false.) print *, self%width
   end function sdl_name

   pure function cstr(s) result(c)
      character(*), intent(in) :: s
      character(len=len_trim(s) + 1, kind=c_char) :: c
      integer :: i

      do i = 1, len_trim(s)
         c(i:i) = s(i:i)
      end do
      c(len_trim(s) + 1:len_trim(s) + 1) = c_null_char
   end function cstr

end module ui_plat_sdl2
