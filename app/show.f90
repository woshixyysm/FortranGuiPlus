!> Windowed demo: SDL2 window + OpenGL 3.3 core backend.
!>
!> This is the "real" front end - the same UI code that runs headless, only
!> the platform and renderer objects change. Borderless by default; the
!> framework draws its own title bar and close box.
!>
!>   build.ps1 -Window        then run out\fui_show.exe
!>   fui_show.exe --decorated to get the OS frame instead
program show
   use, intrinsic :: iso_c_binding
   use, intrinsic :: iso_fortran_env, only: real64
   use ui_kinds
   use ui
   use ui_global
   use ui_geom
   use ui_vector
   use ui_svg
   use ui_font_os
   use ui_theme
   use ui_gfx
   use ui_gfx_gl
   use ui_plat_sdl2
   implicit none

   character(len=*), parameter :: HEART = &
      'M12 21.35l-1.45-1.32C5.4 15.36 2 12.28 2 8.5 2 5.42 4.42 3 7.5 3c1.74 0 '// &
      '3.41.81 4.5 2.09C13.09 3.81 14.76 3 16.5 3 19.58 3 22 5.42 22 8.5c0 3.78'// &
      '-3.4 6.86-8.55 11.54L12 21.35z'

   type(ui_sdl2_backend) :: plat
   type(ui_gl_backend) :: gfx
   type(ui_ctx), pointer :: app
   type(ui_batch) :: batch
   type(ui_batch) :: chrome
   type(ui_input_state) :: inp
   type(ui_path) :: icon
   type(ui_path) :: vtext

   logical :: alive, ok, borderless, have_font, use_root
   logical :: animate = .true.
   logical :: compact = .false., glass = .true.
   integer :: clicks = 0, ios, frames, wmode, theme_idx = 2, radio_sel = 1, nav_sel = 1
   integer :: split, k
   real(wp) :: gain = 0.5_wp, t, prog
   real(wp) :: ms = 16.7_wp, radius = 24.0_wp, ax, ay
   type(ui_theme_t) :: th
   character(len=512) :: arg
   character(len=64) :: drv, want_theme, buf
   integer :: ln

   ! ---------------- window mode ---------------------------------
   ! overlay : borderless, always on top, fixed      (ImGui style)
   ! window  : normal OS decorated, resizable
   ! csd     : borderless + resizable, we draw the frame
   !           (Wayland xdg_toplevel client side decorations)
   wmode = UI_MODE_OVERLAY
   drv = ''
   want_theme = 'md3-dark'
   use_root = .false.
   do ios = 1, command_argument_count()
      call get_command_argument(ios, arg, ln)
      if (ln <= 0) cycle
      select case (arg(1:ln))
      case ('--window')
         wmode = UI_MODE_WINDOW
      case ('--csd')
         wmode = UI_MODE_CSD
      case ('--overlay')
         wmode = UI_MODE_OVERLAY
      case ('--wayland')
         drv = 'wayland'
      case ('--x11')
         drv = 'x11'
      case ('--decorated')
         wmode = UI_MODE_WINDOW
      case ('--bare')
         use_root = .true.
      case default
         if (ln > 8) then
            if (arg(1:8) == '--theme=') want_theme = arg(9:ln)
         end if
      end select
   end do
   select case (trim(want_theme))
   case ('md3', 'md3-light')
      theme_idx = 1
   case ('md3-dark')
      theme_idx = 2
   case ('md3e', 'expressive')
      theme_idx = 5
   case ('miuix')
      theme_idx = 3
   case ('winui', 'mica')
      theme_idx = 4
   end select
   borderless = (wmode /= UI_MODE_WINDOW)

   ok = plat%create(1280, 860, 'fui - Fortran IMGUI', mode=wmode, driver=drv)
   if (.not. ok) then
      write (*, '(a)') 'SDL2: window creation failed'
      error stop 1
   end if

   if (.not. gl_init(resolver)) then
      write (*, '(a)') 'GL: shader/context initialisation failed'
      error stop 2
   end if
   write (*, '(a,i0,a,a)') 'window ready, mode=', wmode, ' driver=', trim(drv)
   write (*, '(a)') 'drag the title bar to move, click the box to quit'

   ! ---------------- ui state -----------------------------------
   app => ui_ctx_current()
   app%win_pos = [40.0_wp, 40.0_wp]
   app%style%text_scale = 2.4_wp
   th = theme_of(theme_idx)
   call app%apply_theme(th)

   call open_font(have_font)
   if (have_font) then
      call ui_set_text_measure(measure_text)
      call ui_set_text_sink(draw_text)
   end if

   ! ---------------- loop ---------------------------------------
   frames = 0
   do
      call plat%poll(inp, alive)
      if (.not. alive) exit
      app%dt = max(plat%dt, 1.0e-3_wp)
      ms = ms*0.9_wp + app%dt*1000.0_wp*0.1_wp

      ! theme is data, so re-applying it every frame is free
      th = theme_of(theme_idx)
      th%radius_panel = radius
      if (.not. glass) then
         th%glow_blur = 0.0_wp
         th%glow_exposure = 0.0_wp
      end if
      call app%apply_theme(th)

      call app%begin_frame(inp)


      if (use_root) then
         ! inset so the rounded corners and the blurred backdrop are visible
         call app%begin_root(40.0_wp, 40.0_wp, real(plat%width, wp) - 80.0_wp, &
                             real(plat%height, wp) - 80.0_wp, 'root')
      else
         call app%begin_window('fui - Fortran IMGUI', 640.0_wp, 420.0_wp, &
                               frameless=borderless, closable=.true.)
         if (app%want_close) exit
      end if

      ! ---- app bar: big title ------------------------------------------
      app%style%text_scale = 2.0_wp
      call app%heading('Now Playing')
      call app%text('9 tracks - 38 min')
      call app%spacing(14.0_wp)

      ! ---- hero: album art as its own glass body -----------------------
      ax = app%win_pos(1) + app%win_w - 300.0_wp
      ay = app%win_pos(2) + 96.0_wp
      call app%dl%round_rect(ax + 3.0_wp, ay + 8.0_wp, 236.0_wp, 236.0_wp, 28.0_wp, &
                             ui_color(0_ip, 0_ip, 0_ip, 90_ip))
      call app%dl%round_rect(ax, ay, 236.0_wp, 236.0_wp, 28.0_wp, &
                             ui_color(60_ip, 34_ip, 96_ip, 255_ip))
      call app%dl%gradient(ax, ay, 236.0_wp, 236.0_wp, &
                           ui_color(206_ip, 148_ip, 255_ip, 235_ip), &
                           ui_color(46_ip, 26_ip, 78_ip, 245_ip), 40)
      call app%dl%glass(ax, ay, 236.0_wp, 236.0_wp, 28.0_wp, &
                        ui_color(255_ip, 255_ip, 255_ip, 40_ip))
      call app%dl%round_rect_outline(ax, ay, 236.0_wp, 236.0_wp, 28.0_wp, &
                                     ui_color(255_ip, 255_ip, 255_ip, 90_ip), 1.5_wp)

      ! ---- list ----------------------------------------------------
      if (app%list_item('1', 'Aurora', 'NeriPlayer - 4:12')) continue
      if (app%list_item('2', 'Glass Garden', 'Fortran sessions - 3:40')) continue
      if (app%list_item('3', 'Midnight Blue', 'Mica mix - 5:02')) continue
      call app%spacing(6.0_wp)

      ! ---- transport -------------------------------------------------
      if (app%button_kind('Play', 1)) animate = .true.
      call app%same_line()
      if (app%button_kind('Shuffle', 2)) continue
      call app%same_line()
      if (app%button_kind('Queue', 3)) continue
      call app%same_line()
      if (app%button_kind('More', 4)) continue
      call app%spacing(6.0_wp)

      call app%slider_float('Gain', gain, 0.0_wp, 1.0_wp)
      if (app%switch('Animate', animate)) continue
      if (app%switch('Liquid', glass)) continue

      call app%spacing(4.0_wp)
      call pick('MD3 light', 1)
      call app%same_line()
      call pick('MD3 dark', 2)
      call app%same_line()
      call pick('MIUIX', 3)
      call app%same_line()
      call pick('Mica', 4)
      call app%same_line()
      call pick('MD3 X', 5)
      call app%spacing(4.0_wp)

      if (animate) then
         prog = modulo(app%time*0.5_wp, 1.0_wp)
         t = 0.5_wp + 0.5_wp*sin(app%time*3.0_wp)
      else
         prog = gain
         t = gain
      end if
      if (prog > 0.0_wp .and. prog < 1.1_wp) continue

      ! ---- floating action button + bottom navigation ---------------
      if (app%fab('+')) clicks = clicks + 1

      if (use_root) then
         call app%end_root()
      else
         call app%end_window()
      end if
      ! nav bar is absolutely positioned, so it must come after the content
      nav_sel = app%nav_bar(['Home  ', 'Srch  ', 'Lib   ', 'Me    '], nav_sel)
      call app%end_frame()

      ! ---------------- geometry + vectors ----------------------
      ! single pass: one draw list, one batch, one draw call
      call batch%build(app%dl, plat%width, plat%height)
      ! ---------------- present --------------------------------
      call gfx%begin(plat%width, plat%height)
      call gfx%draw(batch)
      call gfx%resolve()
      if (frames == 0) then
         write (*, '(a,i0,a,i0,a,i0)') 'first frame: ', batch%count, ' verts, ', &
            plat%width, ' x ', plat%height
      end if
      call plat%swap()
      frames = frames + 1
   end do

   call plat%shutdown()
   write (*, '(a,i0,a)') 'closed after ', frames, ' frames'

contains

   !> GL entry point resolver handed to ui_gfx_gl.
   function resolver(name) result(p)
      character(kind=c_char) :: name(*)
      type(c_ptr) :: p

      p = plat%proc(cstr_from(name))
   end function resolver

   function cstr_from(name) result(s)
      character(kind=c_char), intent(in) :: name(*)
      character(len=128) :: s
      integer :: i

      s = ' '
      do i = 1, 127
         if (name(i) == c_null_char) exit
         s(i:i) = name(i)
      end do
   end function cstr_from

   function i2s(v) result(s)
      integer, intent(in) :: v
      character(len=12) :: s

      write (s, '(i0)') v
   end function i2s

   !> Select a design language by index.
   function theme_of(idx) result(r)
      integer, intent(in) :: idx
      type(ui_theme_t) :: r
      type(ui_color) :: seed

      select case (idx)
      case (1)
         r = theme_md3_light()
      case (2)
         r = theme_md3_dark()
      case (3)
         r = theme_miuix()
      case (4)
         r = theme_winui_mica()
      case (5)
         r = theme_md3_expressive()
      case default
         seed = ui_color(214_ip, 92_ip, 128_ip, 255_ip)
         r = theme_md3_from_seed(seed, .true.)
      end select
   end function theme_of

   !> One entry of the live theme switcher.
   subroutine pick(name, idx)
      character(*), intent(in) :: name
      integer, intent(in) :: idx
      logical :: sel

      sel = (theme_idx == idx)
      if (app%chip(name, sel)) then
         if (sel) theme_idx = idx
      end if
   end subroutine pick

   !> Try the usual font locations through the OS shim.
   subroutine open_font(have)
      logical, intent(out) :: have
      character(len=256) :: cand(5)
      integer :: k

      have = .false.
      cand(1) = 'C:\Windows\Fonts\segoeui.ttf'
      cand(2) = 'C:\Windows\Fonts\arial.ttf'
      cand(3) = '/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf'
      cand(4) = '/System/Library/Fonts/Supplemental/Arial.ttf'
      cand(5) = '/data/data/com.termux/files/usr/share/fonts/TTF/DejaVuSans.ttf'

      do k = 1, 5
         inquire (file=trim(cand(k)), exist=ok)
         if (ok) then
            if (ui_font_open(trim(cand(k)), 34)) then
               have = .true.
               return
            end if
         end if
      end do
   end subroutine open_font

   !> Bitmap scale -> pixel size. Rounded to whole pixels so glyph outlines
   !> land on the raster grid and stay crisp.
   pure function px_of(scale) result(px)
      real(wp), intent(in) :: scale
      real(wp) :: px

      px = real(max(8, nint(7.6_wp*max(scale, 1.0_wp))), wp)
   end function px_of

   !> Layout metrics: real advances, so centred labels sit where they should.
   function measure_text(s, scale) result(w)
      character(*), intent(in) :: s
      real(wp), intent(in) :: scale
      real(wp) :: w

      w = ui_font_text_width(s, px_of(scale))
   end function measure_text

   !> Render a UI string as filled glyph outlines - resolution independent.
   subroutine draw_text(b, x, y, s, col, scale)
      class(ui_batch), intent(inout) :: b
      real(wp), intent(in) :: x, y, scale
      character(*), intent(in) :: s
      type(ui_color), intent(in) :: col
      type(ui_path) :: p

      if (len_trim(s) == 0) return
      call p%reset()
      call ui_font_text_path(s, px_of(scale), aint(x), &
                             aint(y + ui_font_ascent_ratio()*px_of(scale)), p)
      call p%fill(b, col)
   end subroutine draw_text

end program show
