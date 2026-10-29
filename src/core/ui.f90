!> Immediate-mode UI core: layout, hit testing, widgets.
!>
!> Usage contract (identical in spirit to Dear ImGui):
!>
!>   call app%begin_frame(input)
!>   call app%begin_window(title, w, h)
!>      ... widgets ...
!>   call app%end_window()
!>   call app%end_frame()
!>   -> app%dl now holds a renderer independent draw list
!>
!> All widgets are functions/subroutines that are called *every frame*.
!> The only persistent state is what the application owns (checkbox values,
!> slider values, ...) plus a handful of interaction fields inside ui_ctx.
module ui
   use ui_kinds
   use ui_draw
   use ui_input
   use ui_theme
   use ui_anim
   implicit none
   private

   public :: ui_ctx, ui_style, ui_text_width, ui_hash
   public :: ui_vec2, ui_rect, ui_color
   public :: ui_drawlist, ui_draw_cmd, ui_input_state
   public :: UI_DRAW_RECT, UI_DRAW_TEXT, UI_DRAW_LINE, UI_DRAW_CIRCLE
   public :: ui_set_text_measure, ui_measure_text

   !> Unscaled glyph metrics of the built-in font.
   real(wp), parameter :: GLYPH_ADV = 6.0_wp
   real(wp), parameter :: GLYPH_H = 7.0_wp
   !> Title bar height of a window.
   real(wp), parameter :: TITLE_H = 30.0_wp
   !> Depth of the id stack.
   integer, parameter :: ID_MAX = 32

   !> Colours and metrics. Every field is public and can be tweaked.
   type :: ui_style
      type(ui_color) :: bg = ui_color(24_ip, 26_ip, 31_ip, 255_ip)
      type(ui_color) :: panel = ui_color(38_ip, 41_ip, 50_ip, 255_ip)
      type(ui_color) :: title = ui_color(52_ip, 57_ip, 70_ip, 255_ip)
      type(ui_color) :: title_hot = ui_color(62_ip, 68_ip, 84_ip, 255_ip)
      type(ui_color) :: text = ui_color(228_ip, 232_ip, 240_ip, 255_ip)
      type(ui_color) :: text_dim = ui_color(148_ip, 154_ip, 168_ip, 255_ip)
      type(ui_color) :: accent = ui_color(86_ip, 156_ip, 214_ip, 255_ip)
      type(ui_color) :: widget = ui_color(60_ip, 65_ip, 78_ip, 255_ip)
      type(ui_color) :: widget_hot = ui_color(76_ip, 82_ip, 97_ip, 255_ip)
      type(ui_color) :: widget_active = ui_color(86_ip, 156_ip, 214_ip, 255_ip)
      type(ui_color) :: border = ui_color(18_ip, 20_ip, 24_ip, 255_ip)
      type(ui_color) :: shadow = ui_color(8_ip, 9_ip, 12_ip, 110_ip)
      type(ui_color) :: on_primary = ui_color(12_ip, 20_ip, 30_ip, 255_ip)
      type(ui_color) :: primary_container = ui_color(38_ip, 62_ip, 88_ip, 255_ip)
      type(ui_color) :: on_primary_container = ui_color(206_ip, 228_ip, 250_ip, 255_ip)
      type(ui_color) :: outline_soft = ui_color(255_ip, 255_ip, 255_ip, 28_ip)
      !> Shape / effects copied from a ui_theme_t.
      real(wp) :: radius_panel = 16.0_wp
      real(wp) :: radius_widget = 8.0_wp
      real(wp) :: radius_button = 8.0_wp
      real(wp) :: border_w = 0.0_wp
      real(wp) :: glow_blur = 12.0_wp
      real(wp) :: glow_exposure = 0.5_wp
      type(ui_color) :: glow_color = ui_color(255_ip, 236_ip, 244_ip, 160_ip)
      type(ui_color) :: surface_top = ui_color(70_ip, 76_ip, 92_ip, 210_ip)
      type(ui_color) :: surface_bot = ui_color(38_ip, 41_ip, 50_ip, 235_ip)
      logical :: pill = .false.
      logical :: gradient_surface = .true.
      !> Draw the panel as frosted glass over the blurred backdrop.
      logical :: frosted = .true.
      !> Tint strength of the glass (alpha = how much tint over the blur).
      type(ui_color) :: frost_tint = ui_color(24_ip, 26_ip, 34_ip, 150_ip)
      !> Liquid glass: rim highlight + a slow specular sweep across the panel.
      logical :: liquid = .true.
      !> Font scale factor (integer pixel stepping).
      real(wp) :: text_scale = 2.0_wp
      !> M3 type ramp, as multipliers of text_scale. Baseline sizes are
      !> 57/22/16/12 so the contrast between roles is real, not subtle.
      real(wp) :: ts_display = 1.85_wp   ! display/headline
      real(wp) :: ts_title = 1.20_wp     ! title, button label
      real(wp) :: ts_body = 1.00_wp      ! body large
      real(wp) :: ts_label = 0.80_wp     ! label, caption, nav item
      !> Inner padding of a window.
      real(wp) :: pad = 12.0_wp
      !> Gap between items, horizontally and vertically.
      real(wp) :: spacing = 8.0_wp
   end type ui_style

   !> The immediate-mode context.
   type :: ui_ctx
      type(ui_drawlist) :: dl
      type(ui_input_state) :: in
      type(ui_style) :: style
      !> Per-widget eased interaction values, keyed by id.
      type(ui_anim_store) :: anim

      !> Top-left corner of the window; survives frames so it can be dragged.
      real(wp) :: win_pos(2) = [70.0_wp, 40.0_wp]
      !> Bottom navigation bar position; the bar itself can be dragged.
      real(wp) :: nav_pos(2) = [-1.0_wp, -1.0_wp]
      !> True while the nav bar is being dragged.
      logical :: nav_drag = .false.
      !> Horizontal item drag inside the nav bar: the bar stays put, the item
      !> you grab lifts out of it and follows the pointer.
      logical :: nav_item_drag = .false.
      logical :: nav_blob_live = .false.
      real(wp) :: nav_blob_x = 0.0_wp
      real(wp) :: nav_blob_v = 0.0_wp
      integer(ip) :: nav_item_drag_idx = 0_ip
      real(wp) :: nav_item_drag_x = 0.0_wp
      !> Y of the layout origin, used to place the nav bar by default.
      real(wp) :: cur_y0 = 0.0_wp
      !> True while the frameless panel is being dragged by its background.
      logical :: dragging = .false.

      !> Seconds since the first frame, driven by ui_ctx%dt.
      real(wp) :: time = 0.0_wp
      real(wp) :: dt = 1.0_wp/60.0_wp
      integer :: frame = 0

      !> Interaction state (ImGui "hot"/"active").
      integer(ip) :: hot = 0_ip
      integer(ip) :: active = 0_ip

      !> Widget id scope stack.
      integer(ip) :: id_stack(ID_MAX) = 0_ip
      integer :: id_depth = 0

      !> Rect of the most recently submitted item (interactive region).
      type(ui_rect) :: last_rect = ui_rect()

      !> Window chrome state.
      logical :: frameless = .false.
      logical :: want_close = .false.

      !> Layout state, owned by the framework.
      real(wp) :: content_x = 0.0_wp
      real(wp) :: cur_y = 0.0_wp
      real(wp) :: line_y = 0.0_wp
      real(wp) :: line_h = 0.0_wp
      real(wp) :: next_x = 0.0_wp
      logical :: same_line_pending = .false.
      real(wp) :: win_w = 0.0_wp
      real(wp) :: win_h = 0.0_wp
   contains
      procedure :: begin_frame
      procedure :: end_frame
      procedure :: apply_theme
      procedure :: glass_panel => draw_glass_panel
      procedure :: begin_window
      procedure :: end_window
      procedure :: begin_root
      procedure :: end_root
      procedure :: reserve
      procedure :: interact
      procedure :: same_line
      procedure :: spacing => w_spacing
      procedure :: separator => w_separator
      procedure :: text => w_text
      procedure :: button => w_button
      procedure :: button_kind => w_button_kind
      procedure :: icon_button => w_icon_button
      procedure :: fab => w_fab
      procedure :: list_item => w_list_item
      procedure :: nav_bar => w_nav_bar
      procedure :: card => w_card
      procedure :: checkbox => w_checkbox
      procedure :: slider_float => w_slider_float
      procedure :: progress => w_progress
      procedure :: label_value => w_label_value
      procedure :: switch => w_switch
      procedure :: chip => w_chip
      procedure :: radio => w_radio
      procedure :: heading => w_heading
   end type ui_ctx

contains

   !------------------------------------------------------------------
   ! helpers
   !------------------------------------------------------------------

   !> 31 bit FNV-1a style hash; safe from integer overflow by working in i64.
   pure integer(ip) function ui_hash(s, seed) result(h)
      character(*), intent(in) :: s
      integer(ip), intent(in), optional :: seed
      integer(i64) :: x
      integer :: i

      x = 0_i64
      if (present(seed)) x = int(iand(seed, int(z'7FFFFFFF', ip)), i64)
      do i = 1, len_trim(s)
         x = iand(ieor(x, int(iachar(s(i:i)), i64))*16777619_i64, 2147483647_i64)
      end do
      h = int(x, ip)
      if (h == 0_ip) h = 1_ip
   end function ui_hash

   !> Pixel width of a string. Uses the installed font hook when present so
   !> layout matches the real, vector rendered text.
   real(wp) function ui_text_width(s, scale) result(w)
      character(*), intent(in) :: s
      real(wp), intent(in), optional :: scale
      real(wp) :: sc, hw
      integer :: n

      sc = 1.0_wp
      if (present(scale)) sc = max(scale, 1.0_wp)

      hw = ui_measure_text(s, sc)
      if (hw >= 0.0_wp) then
         w = hw
         return
      end if

      n = len_trim(s)
      if (n <= 0) then
         w = 0.0_wp
      else
         w = (real(n, wp)*GLYPH_ADV - 1.0_wp)*sc
      end if
   end function ui_text_width

   subroutine push_id(self, id)
      class(ui_ctx), intent(inout) :: self
      integer(ip), intent(in) :: id

      if (self%id_depth < ID_MAX) then
         self%id_depth = self%id_depth + 1
         self%id_stack(self%id_depth) = id
      end if
   end subroutine push_id

   subroutine pop_id(self)
      class(ui_ctx), intent(inout) :: self

      if (self%id_depth > 0) self%id_depth = self%id_depth - 1
   end subroutine pop_id

   !> Build a widget id, scoped by the current id stack.
   integer(ip) function make_id(self, s, extra) result(id)
      class(ui_ctx), intent(in) :: self
      character(*), intent(in) :: s
      integer, intent(in), optional :: extra
      integer(ip) :: seed
      character(len=32) :: num

      seed = 0_ip
      if (self%id_depth > 0) seed = self%id_stack(self%id_depth)
      if (present(extra)) then
         write (num, '(i0)') extra
         id = ui_hash(trim(s)//'#'//trim(num), seed)
      else
         id = ui_hash(s, seed)
      end if
   end function make_id

   !> Common hit-test / activation logic.
   subroutine interact(self, id, r, hovered, held, clicked)
      class(ui_ctx), intent(inout) :: self
      integer(ip), intent(in) :: id
      type(ui_rect), intent(in) :: r
      logical, intent(out) :: hovered, held, clicked

      hovered = r%contains(self%in%mouse_x, self%in%mouse_y)
      if (hovered) self%hot = id
      if (hovered .and. self%in%mouse_pressed) self%active = id
      held = (self%active == id)
      clicked = held .and. self%in%mouse_released .and. hovered
   end subroutine interact

   !------------------------------------------------------------------
   ! frame / window
   !------------------------------------------------------------------

   !> Liquid glass panel: frosted backdrop, bright rim, inner top glow and a
   !> slow specular band that sweeps across the surface.
   subroutine draw_glass_panel(self, x, y, w, h, radius)
      class(ui_ctx), intent(inout) :: self
      real(wp), intent(in) :: x, y, w, h, radius
      real(wp) :: sh, cw, fx, d, amp
      type(ui_color) :: clear, sheen
      integer :: k, a
      integer, parameter :: NCOL = 48

      call self%dl%glass(x, y, w, h, radius, self%style%frost_tint)
      if (.not. self%style%liquid) return

      ! Nothing is drawn on top of the glass: the shader already produces the
      ! edge highlight. An extra ring here reads as a double border.
      if (.false.) then
         call self%dl%round_rect_outline(x, y, w, h, radius, &
                                         ui_color(0_ip, 0_ip, 0_ip, 0_ip), 1.0_wp)
      end if
   end subroutine draw_glass_panel

   !> Copy a design language into the style. One call switches the whole look.
   subroutine apply_theme(self, t)
      class(ui_ctx), intent(inout) :: self
      type(ui_theme_t), intent(in) :: t

      self%style%panel = t%surface
      self%style%title = t%surface_high
      self%style%title_hot = t%surface_high
      self%style%text = t%on_surface
      self%style%text_dim = t%on_surface_dim
      self%style%accent = t%primary
      self%style%widget = t%surface_low
      self%style%widget_hot = t%surface_high
      self%style%widget_active = t%primary_container
      self%style%border = t%outline
      self%style%outline_soft = t%outline_soft
      self%style%on_primary = t%on_primary
      self%style%primary_container = t%primary_container
      self%style%on_primary_container = t%on_primary_container
      self%style%radius_panel = t%radius_panel
      self%style%radius_widget = t%radius_widget
      self%style%radius_button = t%radius_button
      self%style%border_w = t%border_w
      self%style%pad = t%pad
      self%style%spacing = t%spacing
      self%style%pill = t%pill
      self%style%gradient_surface = t%gradient_surface
      self%style%frosted = t%frosted
      self%style%frost_tint = t%frost_tint
      self%style%liquid = t%liquid
      self%style%glow_blur = t%glow_blur
      self%style%glow_exposure = t%glow_exposure
      self%style%glow_color = t%glow_color
      self%style%surface_top = t%surface_top
      self%style%surface_bot = t%surface_bot
   end subroutine apply_theme

   subroutine begin_frame(self, input)
      class(ui_ctx), intent(inout) :: self
      type(ui_input_state), intent(in) :: input

      self%in = input
      self%frame = self%frame + 1
      self%time = self%time + self%dt
      call self%anim%begin(self%dt)
      self%hot = 0_ip
      self%want_close = .false.
      call self%dl%clear()

      self%id_depth = 0
      self%same_line_pending = .false.
      self%line_h = 0.0_wp
   end subroutine begin_frame

   subroutine end_frame(self)
      class(ui_ctx), intent(inout) :: self

      ! A release (or a lost button) always ends the interaction.
      if (self%in%mouse_released .or. (.not. self%in%mouse_down)) self%active = 0_ip
      call self%anim%step()
   end subroutine end_frame

   !> Open the single window. Position persists in ui_ctx%win_pos and can be
   !> dragged by grabbing the title bar.
   !> `frameless` draws our own chrome instead of an OS decoration: a deeper
   !> shadow, no outline, and a close box. `closable` adds the close box.
   subroutine begin_window(self, title, w, h, frameless, closable)
      class(ui_ctx), intent(inout) :: self
      character(*), intent(in) :: title
      real(wp), intent(in) :: w, h
      logical, intent(in), optional :: frameless, closable
      type(ui_rect) :: tr, close_r
      integer(ip) :: id, cid
      logical :: hovered, held, clicked, chov, cheld, cclicked
      logical :: fless, clos
      real(wp) :: sc, ts, x, y, bw

      fless = .false.
      if (present(frameless)) fless = frameless
      clos = .false.
      if (present(closable)) clos = closable
      self%frameless = fless

      sc = self%style%text_scale
      ts = GLYPH_H*sc

      id = ui_hash(title, 0_ip)
      call push_id(self, id)

      tr = ui_rect(self%win_pos(1), self%win_pos(2), w, TITLE_H)
      call self%interact(id, tr, hovered, held, clicked)
      if (held) then
         self%win_pos(1) = self%win_pos(1) + self%in%mouse_dx
         self%win_pos(2) = self%win_pos(2) + self%in%mouse_dy
      end if

      x = self%win_pos(1)
      y = self%win_pos(2)

      if (fless) then
         ! Frameless: translucent panel, optional Gaussian glow, two-tone body.
         if (self%style%glow_blur > 0.0_wp) then
            call self%dl%glow(x, y, w, h, self%style%radius_panel, &
                              self%style%glow_color, &
                              blur=self%style%glow_blur, &
                              exposure=self%style%glow_exposure)
         end if
         call self%dl%round_rect(x, y, w, h, self%style%radius_panel, &
                                 self%style%panel)
         if (self%style%frosted) then
            call self%glass_panel(x, y, w, h, self%style%radius_panel)
         end if
         if (self%style%gradient_surface) then
            call self%dl%gradient(x, y + self%style%radius_panel, w, &
                                  h - 2.0_wp*self%style%radius_panel, &
                                  self%style%surface_top, &
                                  self%style%surface_bot, 48)
         end if
         call self%dl%round_rect(x, y, w, TITLE_H + self%style%radius_panel, &
                                 self%style%radius_panel, self%style%title)
         call self%dl%rect(x, y + TITLE_H - self%style%radius_panel*0.5_wp, w, &
                           self%style%radius_panel + 8.0_wp, self%style%panel)
         ! hairline + accent underline
         call self%dl%rect(x + self%style%pad, y + TITLE_H, &
                           w - 2.0_wp*self%style%pad, 1.0_wp, &
                           self%style%outline_soft)
         call self%dl%rect(x + self%style%pad, y + TITLE_H, &
                           w - 2.0_wp*self%style%pad, 2.0_wp, &
                           self%style%accent)
      else
         ! Decorated: rounded card + drop shadow + optional 1dp outline.
         call self%dl%round_rect(x + 6.0_wp, y + 8.0_wp, w, h, &
                                 self%style%radius_panel, self%style%shadow)
         call self%dl%round_rect(x, y, w, h, self%style%radius_panel, &
                                 self%style%panel)
         if (self%style%gradient_surface) then
            call self%dl%gradient(x, y + self%style%radius_panel, w, &
                                  h - 2.0_wp*self%style%radius_panel, &
                                  self%style%surface_top, &
                                  self%style%surface_bot, 48)
         end if
         if (hovered) then
            call self%dl%round_rect(x, y, w, TITLE_H + self%style%radius_panel, &
                                    self%style%radius_panel, self%style%title_hot)
         else
            call self%dl%round_rect(x, y, w, TITLE_H + self%style%radius_panel, &
                                    self%style%radius_panel, self%style%title)
         end if
         call self%dl%rect(x, y + TITLE_H - self%style%radius_panel*0.5_wp, w, &
                           self%style%radius_panel + 8.0_wp, self%style%panel)
         if (self%style%border_w > 0.0_wp) then
            call self%dl%round_rect_outline(x, y, w, h, self%style%radius_panel, &
                                            self%style%outline_soft, &
                                            self%style%border_w)
         end if
      end if
      call self%dl%text(x + self%style%pad, y + (TITLE_H - ts)*0.5_wp, &
                        trim(title), self%style%text, sc)

      ! close box, top right
      if (clos) then
         bw = TITLE_H - 8.0_wp
         close_r = ui_rect(x + w - bw - 6.0_wp, y + 4.0_wp, bw, bw)
         cid = ui_hash(title//'#close', id)
         call self%interact(cid, close_r, chov, cheld, cclicked)
         if (cclicked) self%want_close = .true.
         if (cheld) then
            call self%dl%circle(close_r%center_x(), close_r%center_y(), &
                                bw*0.5_wp, ui_color(214_ip, 76_ip, 84_ip, 255_ip))
         else if (chov) then
            call self%dl%circle(close_r%center_x(), close_r%center_y(), &
                                bw*0.5_wp, ui_color(150_ip, 58_ip, 66_ip, 255_ip))
         end if
         call self%dl%line(close_r%x + 6.0_wp, close_r%y + 6.0_wp, &
                           close_r%x + bw - 6.0_wp, close_r%y + bw - 6.0_wp, &
                           self%style%text, 2.0_wp)
         call self%dl%line(close_r%x + bw - 6.0_wp, close_r%y + 6.0_wp, &
                           close_r%x + 6.0_wp, close_r%y + bw - 6.0_wp, &
                           self%style%text, 2.0_wp)
      end if

      self%win_w = w
      self%win_h = h
      self%content_x = x + self%style%pad
      self%cur_y = y + TITLE_H + self%style%pad
      self%line_y = self%cur_y
      self%line_h = 0.0_wp
      self%next_x = self%content_x
      self%same_line_pending = .false.
      self%last_rect = tr
   end subroutine begin_window

   subroutine end_window(self)
      class(ui_ctx), intent(inout) :: self

      call pop_id(self)
   end subroutine end_window

   !> Root layout: widgets are placed directly on the OS window, with no
   !> inner panel and no chrome. This is the "native app" look - the window
   !> itself is the surface, the widgets float on whatever backdrop you draw.
   subroutine begin_root(self, x, y, w, h, title)
      class(ui_ctx), intent(inout) :: self
      real(wp), intent(in) :: x, y, w, h
      character(*), intent(in), optional :: title
      integer(ip) :: id
      real(wp) :: px, py, mx, my
      logical :: inside

      id = ui_hash('root#'//title, 0_ip)
      call push_id(self, id)

      ! The surface itself is NOT draggable. Only individual floating bodies
      ! (the nav bar, a FAB) move when you drag them. Widgets come first, so
      ! a press that lands on a control never turns into a drag.
      px = self%win_pos(1)
      py = self%win_pos(2)

      self%win_w = w
      self%win_h = h
      self%frameless = .true.
      self%content_x = px + self%style%pad
      self%cur_y = py + self%style%pad
      self%cur_y0 = py
      self%line_y = self%cur_y
      self%line_h = 0.0_wp
      self%next_x = self%content_x
      self%same_line_pending = .false.
      self%last_rect = ui_rect(px, py, w, h)

      ! No nested window: the widgets live directly on the surface. Only the
      ! individual glass bodies (nav bar, FAB, cards) draw a backdrop.
      if (.false.) then
         call self%glass_panel(px, py, w, h, self%style%radius_panel)
      end if
      if (.false.) print *, x, y
   end subroutine begin_root

   subroutine end_root(self)
      class(ui_ctx), intent(inout) :: self

      call pop_id(self)
   end subroutine end_root

   !> Reserve space in the layout and return the rect that was allocated.
   !> Use this to draw custom content with the draw list directly.
   function reserve(self, w, h) result(r)
      class(ui_ctx), intent(inout) :: self
      real(wp), intent(in) :: w, h
      type(ui_rect) :: r

      if (.not. self%same_line_pending) then
         if (self%line_h > 0.0_wp) self%cur_y = self%cur_y + self%line_h + self%style%spacing
         self%line_y = self%cur_y
         self%next_x = self%content_x
         self%line_h = 0.0_wp
      end if

      r = ui_rect(self%next_x, self%line_y, w, h)
      self%next_x = self%next_x + w + self%style%spacing
      self%line_h = max(self%line_h, h)
      self%same_line_pending = .false.
      self%last_rect = r
   end function reserve

   !> Put the next widget on the same line as the previous one.
   subroutine same_line(self)
      class(ui_ctx), intent(inout) :: self

      self%same_line_pending = .true.
   end subroutine same_line

   !> Vertical gap of h pixels.
   subroutine w_spacing(self, h)
      class(ui_ctx), intent(inout) :: self
      real(wp), intent(in) :: h
      type(ui_rect) :: r

      r = self%reserve(0.0_wp, h)
   end subroutine w_spacing

   !> Horizontal rule across the window.
   subroutine w_separator(self)
      class(ui_ctx), intent(inout) :: self
      type(ui_rect) :: r
      real(wp) :: w

      w = self%win_w - 2.0_wp*self%style%pad
      r = self%reserve(w, 9.0_wp)
      call self%dl%rect(r%x, r%y + 4.0_wp, r%w, 1.0_wp, self%style%widget)
   end subroutine w_separator

   !------------------------------------------------------------------
   ! widgets
   !------------------------------------------------------------------

   !> Static text.
   subroutine w_text(self, s, col)
      class(ui_ctx), intent(inout) :: self
      character(*), intent(in) :: s
      type(ui_color), intent(in), optional :: col
      type(ui_color) :: c
      type(ui_rect) :: r
      real(wp) :: sc, ts

      sc = self%style%text_scale
      ts = GLYPH_H*sc
      c = self%style%text
      if (present(col)) c = col

      r = self%reserve(ui_text_width(s, sc), ts)
      call self%dl%text(r%x, r%y, s, c, sc)
   end subroutine w_text

   !> Push button. Returns .true. on the frame the click completes.
   function w_button(self, label) result(clicked)
      class(ui_ctx), intent(inout) :: self
      character(*), intent(in) :: label
      logical :: clicked
      type(ui_rect) :: r
      type(ui_color) :: c
      integer(ip) :: id
      logical :: hovered, held
      real(wp) :: sc, ts, w, h, tw, br, hv, pv

      sc = self%style%text_scale
      ts = GLYPH_H*sc
      tw = ui_text_width(label, sc)
      w = max(110.0_wp, tw + 28.0_wp)
      h = max(30.0_wp, ts + 12.0_wp)

      r = self%reserve(w, h)
      id = make_id(self, label)
      call self%interact(id, r, hovered, held, clicked)

      c = self%style%widget
      if (hovered) c = self%style%widget_hot
      if (held) c = self%style%widget_active

      hv = 0.0_wp
      pv = 0.0_wp
      call self%anim%push(id, hovered, held, hv, pv)
      c = ui_mix_color(self%style%widget, self%style%widget_hot, hv)
      c = ui_mix_color(c, self%style%widget_active, pv)
      br = self%style%radius_button
      if (self%style%pill) br = r%h*0.5_wp
      ! press: shrink slightly, like Compose's ripple-less press feedback
      call self%dl%round_rect(r%x + 1.5_wp*pv, r%y + 1.5_wp*pv, &
                              r%w - 3.0_wp*pv, r%h - 3.0_wp*pv, br, c)
      if (pv > 0.01_wp .or. hv > 0.01_wp) then
         call self%dl%round_rect_outline(r%x, r%y, r%w, r%h, br, &
                                         self%style%outline_soft, 1.0_wp)
      end if
      call self%dl%text(r%x + (r%w - tw)*0.5_wp, r%y + (r%h - ts)*0.5_wp, &
                        label, self%style%text, sc)
   end function w_button

   !> Material 3 button variants. kind:
   !>   1 filled, 2 tonal, 3 outlined, 4 text, 5 elevated
   !> Every variant uses a state layer (on_surface at 8% hover / 12% press)
   !> exactly like material3's Surface/ButtonDefaults.
   function w_button_kind(self, label, kind) result(clicked)
      class(ui_ctx), intent(inout) :: self
      character(*), intent(in) :: label
      integer, intent(in) :: kind
      logical :: clicked
      type(ui_rect) :: r
      type(ui_color) :: fill, ink, layer
      integer(ip) :: id
      logical :: hovered, held
      real(wp) :: sc, ts, w, h, tw, br, hv, pv, sl, inset

      sc = self%style%text_scale*self%style%ts_title
      ts = GLYPH_H*sc
      tw = ui_text_width(label, sc)
      h = max(40.0_wp, ts + 16.0_wp)         ! MD3 min height 40dp
      w = max(tw + 48.0_wp, 64.0_wp)         ! 24dp horizontal padding

      r = self%reserve(w, h)
      id = make_id(self, 'btn'//char(48 + mod(kind, 10))//label)
      call self%interact(id, r, hovered, held, clicked)

      hv = 0.0_wp
      pv = 0.0_wp
      call self%anim%push(id, hovered, held, hv, pv)

      fill = self%style%accent
      ink = self%style%on_primary
      select case (kind)
      case (1)
      ! filled: primary / on_primary
      case (2)
         fill = self%style%primary_container
         ink = self%style%on_primary_container
      case (3)
         fill = ui_color(0_ip, 0_ip, 0_ip, 0_ip)
         ink = self%style%accent
      case (4)
         fill = ui_color(0_ip, 0_ip, 0_ip, 0_ip)
         ink = self%style%accent
      case default
         fill = self%style%widget_hot
         ink = self%style%text
      end select

      br = self%style%radius_button
      if (self%style%pill) br = h*0.5_wp

      ! elevated: soft shadow before the fill
      if (kind == 5) then
         call self%dl%round_rect(r%x + 1.0_wp*pv, r%y + 3.0_wp, r%w, r%h, br, &
                                 self%style%shadow)
      end if
      if (fill%a > 0_ip) then
         inset = 1.0_wp*pv            ! subtle press shrink
         call self%dl%round_rect(r%x + inset, r%y + inset, &
                                 r%w - 2.0_wp*inset, r%h - 2.0_wp*inset, br, fill)
      end if
      if (kind == 3) then
         call self%dl%round_rect_outline(r%x, r%y, r%w, r%h, br, &
                                         self%style%outline_soft, 1.0_wp)
      end if

      ! state layer: this is what makes it feel like a real library
      sl = 0.08_wp*hv + 0.12_wp*pv
      if (sl > 0.001_wp) then
         layer = ui_color(ink%r, ink%g, ink%b, int(255.0_wp*min(1.0_wp, sl/0.12_wp)*0.12_wp))
         call self%dl%round_rect(r%x, r%y, r%w, r%h, br, layer)
      end if

      call self%dl%text(r%x + (r%w - tw)*0.5_wp, r%y + (r%h - ts)*0.5_wp, &
                        label, ink, sc)
   end function w_button_kind

   !> Circular icon button (40dp, MD3 icon button).
   function w_icon_button(self, glyph, selected) result(clicked)
      class(ui_ctx), intent(inout) :: self
      character(*), intent(in) :: glyph
      logical, intent(in), optional :: selected
      logical :: clicked
      type(ui_rect) :: r
      type(ui_color) :: fill, ink
      integer(ip) :: id
      logical :: hovered, held, sel
      real(wp) :: sc, ts, d, tw, hv, pv, cx, cy

      sel = .false.
      if (present(selected)) sel = selected
      sc = self%style%text_scale
      ts = GLYPH_H*sc
      d = max(40.0_wp, ts + 20.0_wp)
      tw = ui_text_width(glyph, sc)

      r = self%reserve(d, d)
      id = make_id(self, 'icon#'//glyph)
      call self%interact(id, r, hovered, held, clicked)
      call self%anim%push(id, hovered, held, hv, pv)

      fill = ui_color(0_ip, 0_ip, 0_ip, 0_ip)
      if (sel) fill = self%style%primary_container
      if (hv > 0.01_wp .and. .not. sel) then
         fill = ui_color(self%style%text%r, self%style%text%g, self%style%text%b, &
                         int(20.0_wp*hv))
      end if
      if (fill%a > 0_ip) call self%dl%circle(r%center_x(), r%center_y(), d*0.5_wp, fill)
      if (pv > 0.01_wp) then
         call self%dl%circle(r%center_x(), r%center_y(), d*0.5_wp*0.9_wp, &
                             ui_color(self%style%text%r, self%style%text%g, &
                                      self%style%text%b, int(24.0_wp*pv)))
      end if
      ink = self%style%text
      if (sel) ink = self%style%on_primary_container
      cx = r%center_x() - tw*0.5_wp
      cy = r%y + (d - ts)*0.5_wp
      call self%dl%text(cx, cy, glyph, ink, sc)
   end function w_icon_button

   !> Floating action button: 56dp circle, accent, lifts on press.
   function w_fab(self, glyph) result(clicked)
      class(ui_ctx), intent(inout) :: self
      character(*), intent(in) :: glyph
      logical :: clicked
      type(ui_rect) :: r
      integer(ip) :: id
      logical :: hovered, held
      real(wp) :: sc, ts, d, tw, hv, pv, lift

      sc = self%style%text_scale
      ts = GLYPH_H*sc
      d = 56.0_wp
      tw = ui_text_width(glyph, sc)

      r = self%reserve(d + 8.0_wp, d + 8.0_wp)
      id = make_id(self, 'fab#'//glyph)
      call self%interact(id, r, hovered, held, clicked)
      call self%anim%push(id, hovered, held, hv, pv)
      lift = 2.0_wp*hv + 4.0_wp*pv

      call self%dl%round_rect(r%x + 4.0_wp, r%y + 4.0_wp + lift, d, d, d*0.5_wp, &
                              self%style%shadow)
      ! the button itself is glass, like iOS 26's floating control
      call self%dl%glass(r%x + 4.0_wp, r%y + 4.0_wp - lift, d, d, d*0.5_wp, &
                         ui_color(255_ip, 255_ip, 255_ip, 60_ip))
      call self%dl%circle_outline(r%x + 4.0_wp + d*0.5_wp, r%y + 4.0_wp - lift + d*0.5_wp, &
                                  d*0.5_wp, self%style%outline_soft, 1.0_wp)
      call self%dl%text(r%x + 4.0_wp + (d - tw)*0.5_wp, &
                        r%y + 4.0_wp - lift + (d - ts)*0.5_wp, glyph, &
                        self%style%on_primary, sc)
   end function w_fab

   !> Full width list row: leading glyph, title, subtitle, trailing chevron.
   !> Press feedback is a state layer, like Compose's ListItem.
   function w_list_item(self, glyph, title, subtitle) result(clicked)
      class(ui_ctx), intent(inout) :: self
      character(*), intent(in) :: glyph, title, subtitle
      logical :: clicked
      type(ui_rect) :: r
      integer(ip) :: id
      logical :: hovered, held
      real(wp) :: sc, ts, h, w, hv, pv, gx, cw

      sc = self%style%text_scale*self%style%ts_title
      ts = GLYPH_H*sc
      h = max(64.0_wp, ts*2.0_wp + 18.0_wp)
      w = self%win_w - 2.0_wp*self%style%pad

      r = self%reserve(w, h)
      id = make_id(self, 'item#'//title)
      call self%interact(id, r, hovered, held, clicked)
      call self%anim%push(id, hovered, held, hv, pv)

      if (hv + pv > 0.01_wp) then
         call self%dl%round_rect(r%x, r%y, r%w, r%h, self%style%radius_widget, &
                                 ui_color(self%style%text%r, self%style%text%g, &
                                          self%style%text%b, &
                                          int(22.0_wp*(0.6_wp*hv + pv))))
      end if

      ! leading badge
      cw = h*0.5_wp
      gx = r%x + 12.0_wp
      call self%dl%round_rect(gx, r%y + (h - cw)*0.5_wp, cw, cw, cw*0.32_wp, &
                              self%style%primary_container)
      call self%dl%text(gx + (cw - ui_text_width(glyph, sc))*0.5_wp, &
                        r%y + (h - ts)*0.5_wp, glyph, &
                        self%style%on_primary_container, sc)

      call self%dl%text(gx + cw + 14.0_wp, r%y + h*0.5_wp - ts - 2.0_wp, &
                        title, self%style%text, sc)
      call self%dl%text(gx + cw + 14.0_wp, r%y + h*0.5_wp + 2.0_wp, &
                        subtitle, self%style%text_dim, &
                        self%style%text_scale*self%style%ts_label)
      call self%dl%text(r%x + r%w - 24.0_wp, r%y + (h - ts)*0.5_wp, '>', &
                        self%style%text_dim, sc)
   end function w_list_item

   !> Bottom navigation bar with an animated indicator behind the active slot.
   !> Bottom navigation bar. The bar itself is anchored; the *items* are what
   !> you drag: grab one and it lifts out of the bar as its own glass chip,
   !> follows the pointer horizontally, and snaps into the nearest slot.
   function w_nav_bar(self, items, current) result(picked)
      class(ui_ctx), intent(inout) :: self
      character(*), intent(in) :: items(:)
      integer, intent(in) :: current
      integer :: picked
      type(ui_rect) :: r, chip
      integer(ip) :: id
      logical :: hovered, held, clicked
      real(wp) :: sc, ts, h, w, slot, hv, pv, ind, cx, bx, by, mx, my
      real(wp) :: d, cxp, vx, sx, sy, cyb, wch, hch, neck, tgt, acc
      integer :: i, eff, ns

      sc = self%style%text_scale*self%style%ts_label
      ts = GLYPH_H*sc
      ns = size(items)
      h = max(68.0_wp, ts + 38.0_wp)
      w = min(600.0_wp, self%win_w - 2.0_wp*self%style%pad - 56.0_wp)
      slot = w/real(ns, wp)
      picked = current

      if (self%nav_pos(1) < -0.5_wp) then
         self%nav_pos(1) = self%content_x + (self%win_w - 2.0_wp*self%style%pad - w)*0.5_wp
         self%nav_pos(2) = self%cur_y0 + self%win_h - h - 24.0_wp
      end if
      bx = self%nav_pos(1)
      by = self%nav_pos(2)
      mx = self%in%mouse_x
      my = self%in%mouse_y

      ! ---- start / end an item drag --------------------------------
      if (self%in%mouse_pressed .and. self%active == 0_ip .and. &
          my >= by .and. my <= by + h) then
         i = int((mx - bx)/slot) + 1
         if (i >= 1 .and. i <= ns) then
            self%nav_item_drag = .true.
            self%nav_item_drag_idx = i
            self%nav_item_drag_x = mx
         end if
      end if
      if (.not. self%in%mouse_down .and. self%nav_item_drag) then
         i = nint((self%in%mouse_x - bx - slot*0.5_wp)/slot) + 1
         picked = min(max(i, 1), ns)
         self%nav_item_drag = .false.
      end if
      if (self%nav_item_drag) self%nav_item_drag_x = mx

      eff = current
      if (self%nav_item_drag) eff = self%nav_item_drag_idx

      ! ---- blob physics ---------------------------------------------
      ! While dragging the blob follows the pointer; on release it springs
      ! back to the slot of the picked item (slightly under-damped).
      if (self%nav_item_drag) then
         self%nav_blob_x = mx
         self%nav_blob_v = self%in%mouse_dx/max(self%dt, 1.0e-3_wp)
         self%nav_blob_live = .true.
      else if (self%nav_blob_live) then
         tgt = bx + slot*(real(picked, wp) - 0.5_wp)
         acc = (tgt - self%nav_blob_x)*140.0_wp - self%nav_blob_v*17.0_wp
         self%nav_blob_v = self%nav_blob_v + acc*self%dt
         self%nav_blob_x = self%nav_blob_x + self%nav_blob_v*self%dt
         if (abs(tgt - self%nav_blob_x) < 0.7_wp .and. &
             abs(self%nav_blob_v) < 10.0_wp) then
            self%nav_blob_x = tgt
            self%nav_blob_v = 0.0_wp
            self%nav_blob_live = .false.
         end if
      end if

      ! ---- the bar --------------------------------------------------
      call self%dl%glass(bx, by, w, h, h*0.5_wp, &
                         ui_color(214_ip, 205_ip, 226_ip, 150_ip))
      call self%dl%round_rect_outline(bx, by, w, h, h*0.5_wp, &
                                      ui_color(255_ip, 255_ip, 255_ip, 64_ip), 1.0_wp)

      ! The active item's lens bulges OUT of the capsule (see the reference
      ! shots: the selected tab is a bigger circle that overflows the bar).
      cx = bx + slot*(real(eff, wp) - 0.5_wp)
      d = h + 16.0_wp
      call self%dl%glass(cx - d*0.5_wp, by - 9.0_wp, d, d, d*0.5_wp, &
                         ui_color(220_ip, 212_ip, 232_ip, 118_ip))
      call self%dl%circle_outline(cx, by - 9.0_wp + d*0.5_wp, d*0.5_wp, &
                                  ui_color(255_ip, 255_ip, 255_ip, 72_ip), 1.0_wp)
      do i = 1, ns
         r = ui_rect(bx + slot*real(i - 1, wp), by, slot, h)
         id = ui_hash('nav#'//trim(items(i)), 0_ip)
         call self%interact(id, r, hovered, held, clicked)
         call self%anim%push(id, hovered, held, hv, pv)
         if (clicked .and. .not. self%nav_item_drag) picked = i

         call self%anim%value(id, merge(1.0_wp, 0.0_wp, i == eff), ind)
         ! the bulging lens below IS the indicator; a pill on top of it
         ! would be a second, redundant highlight
         if (.false. .and. ind > 0.02_wp) then
            cx = r%x + slot*0.5_wp
            call self%dl%round_rect(cx - min(slot*0.5_wp - 6.0_wp, 34.0_wp), &
                                    r%y + (h - 32.0_wp)*0.5_wp, &
                                    2.0_wp*min(slot*0.5_wp - 6.0_wp, 34.0_wp), &
                                    32.0_wp, 16.0_wp, &
                                    ui_color(self%style%accent%r, self%style%accent%g, &
                                             self%style%accent%b, int(80.0_wp*ind)))
         end if
         call self%dl%text(r%x + (slot - ui_text_width(items(i), sc))*0.5_wp, &
                           r%y + h*0.5_wp - ts*0.5_wp, items(i), &
                           ui_mix_color(self%style%text_dim, self%style%text, &
                                        max(hv, ind)), sc)
      end do

      ! ---- the liquid blob ------------------------------------------
      ! While you drag an item it detaches from the bar, stretches along the
      ! drag direction (volume preserving), keeps a thin neck to the bar, and
      ! springs back into its slot when released with a little overshoot.
      if (self%nav_blob_live) then
         vx = self%nav_blob_v
         sx = 1.0_wp + min(0.50_wp, abs(vx)*0.0011_wp)
         sy = 1.0_wp/sx
         d = h + 10.0_wp
         cxp = self%nav_blob_x
         cyb = by - 10.0_wp - 5.0_wp*min(1.0_wp, abs(vx)*0.0006_wp)
         wch = d*sx
         hch = d*sy
         ! ---- contact optics -------------------------------------
         ! The raised body sits *on* the bar, so the two glass surfaces
         ! interact: the bar is occluded underneath, a thin meniscus lights
         ! up where they meet, and the body's underside picks that light up.
         call self%dl%round_rect(cxp - wch*0.46_wp, by - 8.0_wp, wch*0.92_wp, &
                                 16.0_wp, 8.0_wp, ui_color(0_ip, 0_ip, 0_ip, 66_ip))
         call self%dl%round_rect(cxp - wch*0.32_wp, by - 3.0_wp, wch*0.64_wp, &
                                 11.0_wp, 5.5_wp, ui_color(0_ip, 0_ip, 0_ip, 46_ip))
         call self%dl%round_rect(cxp - wch*0.34_wp, by + 1.0_wp, wch*0.68_wp, &
                                 3.0_wp, 1.5_wp, ui_color(255_ip, 255_ip, 255_ip, 110_ip))
         call self%dl%circle_outline(cxp, cyb + hch*0.5_wp, hch*0.5_wp, &
                                     ui_color(255_ip, 255_ip, 255_ip, 95_ip), 2.0_wp)
         call self%dl%round_rect(cxp - wch*0.5_wp + 2.0_wp, cyb + 7.0_wp, wch, hch, &
                                 min(wch, hch)*0.5_wp, ui_color(0_ip, 0_ip, 0_ip, 80_ip))
         call self%dl%glass(cxp - wch*0.5_wp, cyb, wch, hch, min(wch, hch)*0.5_wp, &
                            ui_color(255_ip, 255_ip, 255_ip, 80_ip))
         call self%dl%round_rect_outline(cxp - wch*0.5_wp, cyb, wch, hch, &
                                         min(wch, hch)*0.5_wp, &
                                         ui_color(255_ip, 255_ip, 255_ip, 120_ip), 1.5_wp)
         if (self%nav_item_drag) then
            i = self%nav_item_drag_idx
         else
            i = current
         end if
         if (i >= 1 .and. i <= ns) then
            call self%dl%text(cxp - ui_text_width(items(i), sc)*0.5_wp, &
                              cyb + (hch - ts)*0.5_wp, items(i), self%style%text, sc)
         end if
      end if
   end function w_nav_bar

   !> Rounded card container drawn *before* its content.
   subroutine w_card(self, x, y, w, h)
      class(ui_ctx), intent(inout) :: self
      real(wp), intent(in) :: x, y, w, h

      call self%dl%round_rect(x, y, w, h, self%style%radius_panel*0.6_wp, &
                              self%style%widget)
      call self%dl%round_rect_outline(x, y, w, h, self%style%radius_panel*0.6_wp, &
                                      self%style%outline_soft, 1.0_wp)
   end subroutine w_card

   !> Checkbox with a label. Returns .true. when the value was toggled.
   function w_checkbox(self, label, value) result(changed)
      class(ui_ctx), intent(inout) :: self
      character(*), intent(in) :: label
      logical, intent(inout) :: value
      logical :: changed
      type(ui_rect) :: r, box
      integer(ip) :: id
      logical :: hovered, held, clicked
      real(wp) :: sc, ts, bs, h, tw, br

      sc = self%style%text_scale
      ts = GLYPH_H*sc
      bs = max(20.0_wp, ts + 4.0_wp)
      tw = ui_text_width(label, sc)
      h = max(bs, ts)

      r = self%reserve(bs + 10.0_wp + tw, h)
      id = make_id(self, label)
      call self%interact(id, r, hovered, held, clicked)

      changed = .false.
      if (clicked) then
         value = .not. value
         changed = .true.
      end if

      box = ui_rect(r%x, r%y + (h - bs)*0.5_wp, bs, bs)
      br = self%style%radius_widget*0.5_wp
      if (br > bs*0.5_wp) br = bs*0.5_wp
      if (value) then
         call self%dl%round_rect(box%x, box%y, box%w, box%h, br, self%style%accent)
      else if (hovered) then
         call self%dl%round_rect(box%x, box%y, box%w, box%h, br, &
                                 self%style%widget_hot)
      else
         call self%dl%round_rect(box%x, box%y, box%w, box%h, br, self%style%widget)
      end if
      call self%dl%round_rect_outline(box%x, box%y, box%w, box%h, br, &
                                      self%style%border, 1.5_wp)

      if (value) then
         call self%dl%line(box%x + 0.25_wp*bs, box%y + 0.50_wp*bs, &
                           box%x + 0.42_wp*bs, box%y + 0.70_wp*bs, &
                           self%style%text, 3.0_wp)
         call self%dl%line(box%x + 0.42_wp*bs, box%y + 0.70_wp*bs, &
                           box%x + 0.76_wp*bs, box%y + 0.28_wp*bs, &
                           self%style%text, 3.0_wp)
      end if

      call self%dl%text(box%x + bs + 10.0_wp, r%y + (h - ts)*0.5_wp, &
                        label, self%style%text, sc)
      self%last_rect = r
   end function w_checkbox

   !> Horizontal slider. The optional `changed` flag reports whether the
   !> value was modified during this frame.
   subroutine w_slider_float(self, label, value, vmin, vmax, changed)
      class(ui_ctx), intent(inout) :: self
      character(*), intent(in) :: label
      real(wp), intent(inout) :: value
      real(wp), intent(in) :: vmin, vmax
      logical, intent(out), optional :: changed
      type(ui_rect) :: r, track
      integer(ip) :: id
      logical :: hovered, held, clicked
      type(ui_color) :: knob
      real(wp) :: sc, ts, h, w, t, old
      character(len=32) :: buf

      sc = self%style%text_scale
      ts = GLYPH_H*sc
      h = max(24.0_wp, ts + 8.0_wp)
      w = 384.0_wp

      r = self%reserve(w, h)

      track = ui_rect(r%x + 120.0_wp, r%y + (h - 8.0_wp)*0.5_wp, 200.0_wp, 8.0_wp)
      id = make_id(self, label)
      call self%interact(id, track, hovered, held, clicked)

      old = value
      if (held .and. track%w > 0.0_wp) then
         t = ui_clamp((self%in%mouse_x - track%x)/track%w, 0.0_wp, 1.0_wp)
         value = vmin + t*(vmax - vmin)
         if (value < vmin) value = vmin
         if (value > vmax) value = vmax
      end if
      if (present(changed)) changed = (abs(value - old) > 1.0e-12_wp)
      t = 0.0_wp
      if (vmax > vmin) t = ui_clamp((value - vmin)/(vmax - vmin), 0.0_wp, 1.0_wp)

      call self%dl%text(r%x, r%y + (h - ts)*0.5_wp, label, self%style%text_dim, sc)
      call self%dl%round_rect(track%x, track%y, track%w, track%h, &
                              track%h*0.5_wp, self%style%widget)
      if (track%w*t > track%h) then
         call self%dl%round_rect(track%x, track%y, track%w*t, track%h, &
                                 track%h*0.5_wp, self%style%accent)
      else if (t > 0.0_wp) then
         call self%dl%circle(track%x + track%h*0.5_wp, track%y + track%h*0.5_wp, &
                             track%h*0.5_wp, self%style%accent)
      end if
      if (held) then
         knob = self%style%text
      else
         knob = self%style%widget_hot
      end if
      call self%dl%round_rect(track%x + track%w*t - 9.0_wp, &
                              track%y + 0.5_wp*track%h - 9.0_wp, 18.0_wp, 18.0_wp, &
                              9.0_wp, knob)
      call self%dl%round_rect_outline(track%x + track%w*t - 9.0_wp, &
                                      track%y + 0.5_wp*track%h - 9.0_wp, &
                                      18.0_wp, 18.0_wp, 9.0_wp, &
                                      self%style%border, 1.5_wp)

      write (buf, '(f6.2)') value
      call self%dl%text(r%x + w - 52.0_wp, r%y + (h - ts)*0.5_wp, &
                        trim(adjustl(buf)), self%style%text, sc)

      self%last_rect = track
   end subroutine w_slider_float

   !> Progress bar with a label; fraction is clamped to [0,1].
   subroutine w_progress(self, label, fraction)
      class(ui_ctx), intent(inout) :: self
      character(*), intent(in) :: label
      real(wp), intent(in) :: fraction
      type(ui_rect) :: r
      real(wp) :: sc, ts, h, w, f, tw, fe
      integer(ip) :: eid
      character(len=32) :: buf

      sc = self%style%text_scale
      ts = GLYPH_H*sc
      h = max(28.0_wp, ts + 12.0_wp)
      w = self%win_w - 2.0_wp*self%style%pad
      f = ui_clamp(fraction, 0.0_wp, 1.0_wp)

      r = self%reserve(w, h)
      eid = ui_hash('progress#'//label, 0_ip)
      call self%anim%value(eid, f, fe)
      if (fe < 0.0_wp) fe = 0.0_wp
      if (fe > 1.0_wp) fe = 1.0_wp
      call self%dl%round_rect(r%x, r%y, r%w, r%h, h*0.5_wp, self%style%widget)
      if (r%w*fe > h) then
         call self%dl%round_rect(r%x, r%y, r%w*fe, r%h, h*0.5_wp, self%style%accent)
      else if (fe > 0.0_wp) then
         call self%dl%circle(r%x + h*0.5_wp, r%y + h*0.5_wp, h*0.5_wp, self%style%accent)
      end if
      call self%dl%round_rect_outline(r%x, r%y, r%w, r%h, h*0.5_wp, &
                                      self%style%outline_soft, 1.0_wp)

      write (buf, '(i3)') int(f*100.0_wp)
      tw = ui_text_width(trim(adjustl(buf))//'%', sc)
      call self%dl%text(r%x + self%style%pad, r%y + (r%h - ts)*0.5_wp, label, &
                        self%style%text, sc)
      call self%dl%text(r%x + r%w - tw - self%style%pad, r%y + (r%h - ts)*0.5_wp, &
                        trim(adjustl(buf))//'%', self%style%text, sc)
   end subroutine w_progress

   !> MD3 style switch: 52x32 pill track, animated-looking knob.
   function w_switch(self, label, value) result(changed)
      class(ui_ctx), intent(inout) :: self
      character(*), intent(in) :: label
      logical, intent(inout) :: value
      logical :: changed
      type(ui_rect) :: r, trk
      integer(ip) :: id
      logical :: hovered, held, clicked
      real(wp) :: sc, ts, h, tw, knob, kx, ky, kn
      type(ui_color) :: track_c

      sc = self%style%text_scale
      ts = GLYPH_H*sc
      tw = ui_text_width(label, sc)
      h = max(32.0_wp, ts)
      knob = 24.0_wp

      r = self%reserve(52.0_wp + 10.0_wp + tw, h)
      id = make_id(self, label)
      call self%interact(id, r, hovered, held, clicked)

      changed = .false.
      if (clicked) then
         value = .not. value
         changed = .true.
      end if

      trk = ui_rect(r%x, r%y + (h - 32.0_wp)*0.5_wp, 52.0_wp, 32.0_wp)
      call self%anim%value(id, merge(1.0_wp, 0.0_wp, value), kn)
      track_c = ui_mix_color(self%style%widget_hot, self%style%accent, kn)
      call self%dl%round_rect(trk%x, trk%y, trk%w, trk%h, 16.0_wp, track_c)
      call self%dl%round_rect_outline(trk%x, trk%y, trk%w, trk%h, 16.0_wp, &
                                      self%style%border, 2.0_wp)
      ! knob slides between the two ends; it also grows a little when on
      knob = 16.0_wp + 8.0_wp*kn
      kx = trk%x + 4.0_wp + (trk%w - 8.0_wp - 24.0_wp)*kn
      ky = trk%y + (trk%h - knob)*0.5_wp
      call self%dl%circle(kx + knob*0.5_wp, ky + knob*0.5_wp, knob*0.5_wp, &
                          ui_mix_color(self%style%text_dim, self%style%on_primary, kn))

      call self%dl%text(trk%x + trk%w + 10.0_wp, r%y + (h - ts)*0.5_wp, &
                        label, self%style%text, sc)
      self%last_rect = r
   end function w_switch

   !> Filter chip / pill toggle. Returns the new state.
   function w_chip(self, label, selected) result(changed)
      class(ui_ctx), intent(inout) :: self
      character(*), intent(in) :: label
      logical, intent(inout) :: selected
      logical :: changed
      type(ui_rect) :: r
      integer(ip) :: id
      logical :: hovered, held, clicked
      real(wp) :: sc, ts, tw, h, hv, cv
      type(ui_color) :: fill, ink

      sc = self%style%text_scale*self%style%ts_label
      ts = GLYPH_H*sc
      tw = ui_text_width(label, sc)
      h = max(30.0_wp, ts + 14.0_wp)

      r = self%reserve(tw + 28.0_wp, h)
      id = make_id(self, 'chip#'//label)
      call self%interact(id, r, hovered, held, clicked)
      call self%anim%value(id, merge(1.0_wp, 0.0_wp, selected), cv)

      changed = .false.
      if (clicked) then
         selected = .not. selected
         changed = .true.
      end if

      fill = self%style%widget
      ink = self%style%text
      if (selected) then
         fill = self%style%primary_container
         ink = self%style%on_primary_container
      end if
      call self%anim%push(id, hovered, held, hv)
      if (.not. selected) fill = ui_mix_color(fill, self%style%widget_hot, hv)
      fill = ui_mix_color(fill, self%style%primary_container, cv)
      call self%dl%round_rect(r%x, r%y, r%w, r%h, r%h*0.5_wp, fill)
      call self%dl%round_rect_outline(r%x, r%y, r%w, r%h, r%h*0.5_wp, &
                                      self%style%border, 1.0_wp)
      call self%dl%text(r%x + (r%w - tw)*0.5_wp, r%y + (r%h - ts)*0.5_wp, &
                        label, ink, sc)
      self%last_rect = r
   end function w_chip

   !> Radio button. `selected` is the currently selected index of the group;
   !> clicking sets it to `mine`.
   function w_radio(self, label, selected, mine) result(changed)
      class(ui_ctx), intent(inout) :: self
      character(*), intent(in) :: label
      integer(ip), intent(inout) :: selected
      integer(ip), intent(in) :: mine
      logical :: changed
      type(ui_rect) :: r, dot
      integer(ip) :: id
      logical :: hovered, held, clicked
      real(wp) :: sc, ts, bs, h, tw, cx, cy

      sc = self%style%text_scale
      ts = GLYPH_H*sc
      bs = max(20.0_wp, ts + 4.0_wp)
      tw = ui_text_width(label, sc)
      h = max(bs, ts)

      r = self%reserve(bs + 10.0_wp + tw, h)
      id = make_id(self, 'radio#'//label)
      call self%interact(id, r, hovered, held, clicked)

      changed = .false.
      if (clicked .and. selected /= mine) then
         selected = mine
         changed = .true.
      end if

      cx = r%x + bs*0.5_wp
      cy = r%y + h*0.5_wp
      if ((selected == mine) .or. hovered) then
         call self%dl%circle(cx, cy, bs*0.5_wp, self%style%widget_hot)
      else
         call self%dl%circle(cx, cy, bs*0.5_wp, self%style%widget)
      end if
      call self%dl%circle_outline(cx, cy, bs*0.5_wp, self%style%border, 2.0_wp)
      if (selected == mine) then
         call self%dl%circle(cx, cy, bs*0.25_wp, self%style%accent)
      end if

      call self%dl%text(r%x + bs + 10.0_wp, r%y + (h - ts)*0.5_wp, &
                        label, self%style%text, sc)
      self%last_rect = r
   end function w_radio

   !> Section heading: larger text plus a short accent rule.
   subroutine w_heading(self, label)
      class(ui_ctx), intent(inout) :: self
      character(*), intent(in) :: label
      type(ui_rect) :: r
      real(wp) :: sc, ts, w

      sc = self%style%text_scale*self%style%ts_display
      ts = GLYPH_H*sc
      w = self%win_w - 2.0_wp*self%style%pad
      r = self%reserve(w, ts + 6.0_wp)
      call self%dl%text(r%x, r%y, label, self%style%text, sc)
      call self%dl%rect(r%x, r%y + ts + 2.0_wp, min(48.0_wp, ui_text_width(label, sc)*0.5_wp), &
                        2.0_wp, self%style%accent)
   end subroutine w_heading

   !> "Name ............ value" row, value right aligned.
   subroutine w_label_value(self, label, value)
      class(ui_ctx), intent(inout) :: self
      character(*), intent(in) :: label
      character(*), intent(in) :: value
      type(ui_rect) :: r
      real(wp) :: sc, ts, w, tw

      sc = self%style%text_scale
      ts = GLYPH_H*sc
      w = self%win_w - 2.0_wp*self%style%pad

      r = self%reserve(w, ts)
      call self%dl%text(r%x, r%y, label, self%style%text_dim, sc)
      tw = ui_text_width(value, sc)
      call self%dl%text(r%x + r%w - tw, r%y, value, self%style%text, sc)
   end subroutine w_label_value

end module ui
