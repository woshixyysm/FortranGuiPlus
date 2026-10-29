!> fui demo.
!>
!> There is no window here: a scripted "robot" feeds synthetic mouse events,
!> and the software backend turns the result into images. Everything below the
!> present step is backend agnostic - swapping ui_sw_backend for
!> ui_vk_backend / ui_gl_backend / ui_d3d11_backend is the only change needed
!> to run on a real GPU.
!>
!> Demonstrated:
!>   * immediate-mode widgets (button, checkbox, slider, progress)
!>   * the geometry batcher shared by every GPU backend
!>   * SVG path data filled as vector geometry (lines + cubic curves)
!>   * a TrueType font rasterised from its quadratic outlines
program main
   use, intrinsic :: iso_fortran_env, only: real64
   use ui_kinds
   use ui
   use ui_global
   use ui_geom
   use ui_vector
   use ui_svg
   use ui_font_ttf
   use ui_color_hct
   use ui_img
   use ui_gfx
   use ui_gfx_sw
   implicit none

   integer, parameter :: NF = 40
   integer, parameter :: IW = 860
   integer, parameter :: IH = 660

   ! Material Design heart, 24x24 viewBox: lines plus cubic curves.
   character(len=*), parameter :: ICON_HEART = &
      'M12 21.35l-1.45-1.32C5.4 15.36 2 12.28 2 8.5 2 5.42 4.42 3 7.5 3c1.74 0 '// &
      '3.41.81 4.5 2.09C13.09 3.81 14.76 3 16.5 3 19.58 3 22 5.42 22 8.5c0 3.78'// &
      '-3.4 6.86-8.55 11.54L12 21.35z'

   type(ui_ctx), pointer :: app
   type(ui_input_state) :: inp
   type(ui_sw_backend) :: gfx
   type(ui_sw_backend) :: ref
   type(ui_batch) :: batch
   type(ui_path) :: icon
   type(ui_path) :: vectext
   type(ui_ttf) :: font

   type(ui_rect) :: last_varea

   logical :: show_help = .true.
   logical :: animate = .true.
   real(wp) :: gain = 0.45_wp
   real(wp) :: thresh = 0.25_wp
   integer :: clicks = 0
   integer :: prev_clicks = 0

   type(ui_rect) :: rc_click, rc_help, rc_check, rc_gain, rc_thresh
   type(ui_rect) :: canvas, varea
   real(wp) :: mx, my, pmx, pmy, t, prog, bh, bx
   logical :: mdown, mpressed, mreleased
   integer :: f, i, j, x, y, ios, nframes_written, ndiff, maxdiff, d
   character(len=256) :: fn
   character(len=:), allocatable :: fontfile

   !-------------------------------------------------------------------
   ! setup
   !-------------------------------------------------------------------
   ! The context is not declared here: it is the lazily created singleton.
   ! The first call wires the SAVE'd static pointer, C-style:
   !     static ui_ctx* g = NULL;  if (!g) g = &default_ctx;  return g;
   app => ui_ctx_current()

   app%win_pos = [70.0_wp, 40.0_wp]
   app%dt = 1.0_wp/60.0_wp
   last_varea = ui_rect(70.0_wp, 400.0_wp, 696.0_wp, 96.0_wp)

   call gfx%begin(IW, IH)
   call ref%begin(IW, IH)
   gfx%clear = app%style%bg
   ref%clear = app%style%bg

   call find_font(fontfile)
   if (len(fontfile) > 0) then
      call ui_ttf_load(font, fontfile, ios)
      if (ios /= 0) then
         write (*, '(a,a,a,i0,a)') 'font: cannot parse ', trim(fontfile), ' (', ios, ')'
         write (*, *) '  probe: upem=', font%units_per_em, ' glyphs=', &
            font%num_glyphs, ' loca=', font%off_loca, ' glyf=', font%off_glyf, &
            ' fmt=', font%loca_format
         call ui_ttf_free(font)
      else
         write (*, '(a,a)') 'font: ', trim(fontfile)
         block
            type(ui_path) :: probe
            real(wp) :: bx0, by0, bx1, by1
            integer :: g1
            g1 = font%glyph_index(iachar('H'))
            call probe%reset()
            call ui_ttf_text_path(font, 'HH', 100.0_wp, 0.0_wp, 0.0_wp, probe)
            call probe%bounds(0.1_wp, bx0, by0, bx1, by1)
            write (*, '(a,f8.2,a,f8.2,a,f8.2,a,f8.2)') '  HH: path x=[', bx0, &
               '..', bx1, '] w=', bx1 - bx0, ' adv=', &
               ui_ttf_advance(font, g1)*100.0_wp/font%units_per_em
            write (*, '(a,f8.2,a,i0)') '  text_width=', &
               ui_ttf_text_width(font, 'HH', 100.0_wp), ' gidH=', g1
            call ui_ttf_glyph_debug(font, font%glyph_index(iachar('o')))
         end block
      end if
   else
      write (*, '(a)') 'font: no TrueType file found, vector text skipped'
   end if

   rc_click = ui_rect(-1.0e5_wp, -1.0e5_wp, 0.0_wp, 0.0_wp)
   rc_help = rc_click
   rc_check = rc_click
   rc_gain = rc_click
   rc_thresh = rc_click

   pmx = 0.0_wp
   pmy = 0.0_wp
   nframes_written = 0

   write (*, '(a)') 'fui demo - immediate-mode GUI in pure Fortran 2023'
   write (*, '(a)') '-------------------------------------------------'

   !-------------------------------------------------------------------
   ! frame loop
   !-------------------------------------------------------------------
   do f = 1, NF

      ! ---------- scripted input -------------------------------------
      mx = -1.0e5_wp
      my = -1.0e5_wp
      mdown = .false.
      mpressed = .false.
      mreleased = .false.

      if (f >= 3 .and. f <= 8) then
         mx = rc_click%center_x()
         my = rc_click%center_y()
         if (f == 5) then
            mdown = .true.
            mpressed = .true.
         else if (f == 6) then
            mdown = .true.
         else if (f == 7) then
            mreleased = .true.
         end if
      else if (f >= 11 .and. f <= 23) then
         t = real(min(f, 22) - 11, wp)/11.0_wp
         mx = rc_gain%x + rc_gain%w*ui_clamp(t, 0.0_wp, 1.0_wp)
         my = rc_gain%center_y()
         if (f == 23) then
            mreleased = .true.
         else
            mdown = .true.
            if (f == 11) mpressed = .true.
         end if
      else if (f == 24 .or. f == 25) then
         mx = rc_check%x + 10.0_wp
         my = rc_check%center_y()
         if (f == 24) then
            mdown = .true.
            mpressed = .true.
         else
            mreleased = .true.
         end if
      else if (f == 28 .or. f == 29) then
         mx = rc_help%center_x()
         my = rc_help%center_y()
         if (f == 28) then
            mdown = .true.
            mpressed = .true.
         else
            mreleased = .true.
         end if
      else if (f >= 32 .and. f <= 39) then
         t = 1.0_wp - real(min(f, 38) - 32, wp)/6.0_wp*0.9_wp
         mx = rc_thresh%x + rc_thresh%w*ui_clamp(t, 0.05_wp, 0.95_wp)
         my = rc_thresh%center_y()
         if (f == 39) then
            mreleased = .true.
         else
            mdown = .true.
            if (f == 32) mpressed = .true.
         end if
      end if

      inp%mouse_x = mx
      inp%mouse_y = my
      inp%mouse_dx = mx - pmx
      inp%mouse_dy = my - pmy
      inp%mouse_down = mdown
      inp%mouse_pressed = mpressed
      inp%mouse_released = mreleased
      pmx = mx
      pmy = my

      ! ---------- build the UI ---------------------------------------
      call app%begin_frame(inp)
      call app%begin_window('fui - Fortran IMGUI', 720.0_wp, 430.0_wp, &
                            frameless=.true., closable=.true.)
      if (app%want_close) then
         write (*, '(a)') '  close box clicked, stopping'
         exit
      end if

      call app%text('Immediate mode, Fortran 2023, pluggable GPU backends.')
      call app%spacing(4.0_wp)

      if (app%button('Click me')) clicks = clicks + 1
      rc_click = app%last_rect
      call app%same_line()
      if (app%button('Toggle help')) show_help = .not. show_help
      rc_help = app%last_rect

      if (app%checkbox('Animate the progress bar', animate)) then
         write (*, '(a,i3,a,l1)') '  frame ', f, ': animate -> ', animate
      end if
      rc_check = app%last_rect

      call app%slider_float('Gain', gain, 0.0_wp, 1.0_wp)
      rc_gain = app%last_rect
      call app%slider_float('Threshold', thresh, 0.0_wp, 1.0_wp)
      rc_thresh = app%last_rect

      if (animate) then
         prog = modulo(app%time*0.6_wp, 1.0_wp)
      else
         prog = gain
      end if
      call app%progress('Loading', prog)

      call app%separator()
      call app%label_value('Clicks', trim(int2str(clicks)))
      call app%label_value('Gain', trim(fmt2(gain)))
      call app%label_value('Threshold', trim(fmt2(thresh)))
      if (show_help) then
         call app%label_value('Help', 'visible')
      else
         call app%label_value('Help', 'hidden')
      end if

      ! custom content drawn straight into the draw list
      canvas = app%reserve(696.0_wp, 84.0_wp)
      call app%dl%rect(canvas%x, canvas%y, canvas%w, canvas%h, &
                       ui_color(30_ip, 33_ip, 40_ip, 255_ip))
      call app%dl%rect_outline(canvas%x, canvas%y, canvas%w, canvas%h, &
                               app%style%border, 1.0_wp)
      do i = 1, 24
         bh = 8.0_wp + 58.0_wp*abs(sin(real(i, wp)*0.45_wp + app%time*2.0_wp))
         bx = canvas%x + 10.0_wp + real(i - 1, wp)*28.0_wp
         call app%dl%rect(bx, canvas%y + canvas%h - 8.0_wp - bh, 18.0_wp, bh, &
                          app%style%accent)
      end do

      ! slot reserved for the vector content
      varea = app%reserve(696.0_wp, 96.0_wp)

      call app%end_window()
      call app%end_frame()

      ! ---------- present --------------------------------------------
      call ref%render_drawlist(app%dl)

      if ((f <= 2) .or. (mod(f, 6) == 0) .or. (f == NF)) then
         write (fn, '(a,i3.3,a)') 'frame_', f, '.bmp'
         call ui_save_bmp(ref%img, trim(fn), ios)
         if (ios == 0) then
            nframes_written = nframes_written + 1
            write (*, '(a,i3,a,i5,a,a)') '  frame ', f, ': ', app%dl%count, &
               ' draw cmds -> ', trim(fn)
         end if
      end if

      if (clicks /= prev_clicks) then
         write (*, '(a,i3,a,i0)') '  frame ', f, ': button clicked, count = ', clicks
         prev_clicks = clicks
      end if

      last_varea = varea
   end do

   !-------------------------------------------------------------------
   ! geometry batch, vector content, cross-check
   !-------------------------------------------------------------------
   call batch%build(app%dl, IW, IH)
   call gfx%draw(batch)

   ndiff = 0
   maxdiff = 0
   do y = 1, IH
      do x = 1, IW
         d = abs(ref%img%px(x, y, 1) - gfx%img%px(x, y, 1)) + &
             abs(ref%img%px(x, y, 2) - gfx%img%px(x, y, 2)) + &
             abs(ref%img%px(x, y, 3) - gfx%img%px(x, y, 3))
         if (d > 24) ndiff = ndiff + 1
         maxdiff = max(maxdiff, d)
      end do
   end do

   ! SVG icon: parse the path data, place it in the reserved slot, fill it.
   call icon%reset()
   call ui_svg_path(ICON_HEART, icon)
   call icon%transform(2.6_wp, last_varea%x + 12.0_wp, last_varea%y + 8.0_wp)
   call icon%fill(batch, ui_color(232_ip, 92_ip, 108_ip, 255_ip))

   ! TrueType text: real quadratic outlines, no bitmap font involved.
   if (font%loaded) then
      call vectext%reset()
      call ui_ttf_text_path(font, 'Vector text 44px', 44.0_wp, &
                            last_varea%x + 90.0_wp, last_varea%y + 62.0_wp, vectext)
      call vectext%fill(batch, app%style%text)

      call vectext%reset()
      call ui_ttf_text_path(font, 'quadratic Bezier outlines', 15.0_wp, &
                            last_varea%x + 92.0_wp, last_varea%y + 84.0_wp, vectext)
      call vectext%stroke(batch, app%style%text_dim, 0.9_wp)
   end if

   call gfx%draw(batch)
   call ui_save_bmp(gfx%img, 'frame_vect.bmp', ios)
   call ui_save_ppm(gfx%img, 'frame_vect.ppm', ios)

   write (*, '(a)') ''
   write (*, '(a,i0,a)') 'wrote ', nframes_written, ' widget frames (frame_NNN.bmp)'
   write (*, '(a,i0,a,f6.3,a,f6.3,a,l1)') 'final state: clicks=', clicks, &
      ' gain=', gain, ' threshold=', thresh, ' animate=', animate
   write (*, '(a,i0,a,i0,a,i0,a)') 'batch: ', batch%count, ' vertices, ', &
      batch%count/3, ' triangles, ', ui_vertex_bytes(), ' bytes/vertex'
   write (*, '(a,i0,a,i0,a)') 'vector: SVG ', icon%count(), &
      ' segments, TrueType ', vectext%count(), ' segments'
   write (*, '(a,i0,a,es9.2,a,f6.3,a)') 'drawlist vs batch: ', ndiff, &
      ' differing pixels, max delta ', real(maxdiff, wp), ' (', &
      100.0_wp*real(ndiff, wp)/real(IW*IH, wp), '%)'
   write (*, '(a)') 'vector frame -> frame_vect.bmp'

   block
      type(ui_scheme) :: sc
      type(ui_color) :: seed
      type(ui_hct) :: hh
      seed = ui_color(103_ip, 80_ip, 164_ip, 255_ip)   ! MD3 baseline #6750A4
      hh = ui_hct_from_rgb(seed)
      write (*, '(a,f7.2,a,f7.2,a,f7.2)') 'hct(seed) hue=', hh%hue, &
         ' chroma=', hh%chroma, ' tone=', hh%tone
      sc = ui_scheme_from_seed(seed)
      write (*, '(a,3i4,a,3i4,a,3i4)') 'monet light: primary=', sc%primary%r, &
         sc%primary%g, sc%primary%b, ' surface=', sc%surface%r, sc%surface%g, &
         sc%surface%b, ' on_surface=', sc%on_surface%r, sc%on_surface%g, &
         sc%on_surface%b
      sc = ui_scheme_from_seed(seed, .true.)
      write (*, '(a,3i4,a,3i4)') 'monet dark : primary=', sc%primary%r, &
         sc%primary%g, sc%primary%b, ' surface=', sc%surface%r, sc%surface%g, &
         sc%surface%b
   end block

   call gfx%shutdown()
   call ref%shutdown()

contains

   function int2str(v) result(s)
      integer, intent(in) :: v
      character(len=12) :: s

      write (s, '(i0)') v
   end function int2str

   function fmt2(v) result(s)
      real(wp), intent(in) :: v
      character(len=12) :: s

      write (s, '(f6.3)') v
   end function fmt2

   !> Locate a TrueType file: argv(1), then a few well known locations.
   subroutine find_font(path)
      character(len=:), allocatable, intent(out) :: path
      character(len=512) :: arg
      character(len=512) :: cand(6)
      logical :: ex
      integer :: k, nargs, ln

      path = ''
      nargs = command_argument_count()
      if (nargs >= 1) then
         call get_command_argument(1, arg, ln)
         if (ln > 0) then
            inquire (file=arg(1:ln), exist=ex)
            if (ex) then
               path = arg(1:ln)
               return
            end if
         end if
      end if

      cand(1) = 'C:\Windows\Fonts\segoeui.ttf'
      cand(2) = 'C:\Windows\Fonts\arial.ttf'
      cand(3) = '/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf'
      cand(4) = '/usr/share/fonts/TTF/DejaVuSans.ttf'
      cand(5) = '/System/Library/Fonts/Supplemental/Arial.ttf'
      cand(6) = '/data/data/com.termux/files/usr/share/fonts/TTF/DejaVuSans.ttf'

      do k = 1, 6
         inquire (file=trim(cand(k)), exist=ex)
         if (ex) then
            path = trim(cand(k))
            return
         end if
      end do
   end subroutine find_font

end program main
