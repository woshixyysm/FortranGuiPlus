!> Per-widget animation state for an immediate-mode UI.
!>
!> Immediate mode has no place to keep "how far along is the hover fade", so
!> widgets ask for it by id. Each slot eases its value towards a target set by
!> the widget this frame; the integration happens once in end_frame.
!>
!> Rates are exponential (`v += (t - v) * (1 - exp(-dt*k))`), so the motion is
!> frame-rate independent and never overshoots.
module ui_anim
   use ui_kinds
   implicit none
   private

   public :: ui_anim_store, ui_anim_slot
   public :: ui_mix_color, ui_ease_out
   public :: UI_K_HOVER, UI_K_PRESS, UI_K_VALUE, UI_RIPPLE_TIME

   !> Slots are a small open-addressed table - a UI never has many live items.
   integer, parameter :: ANIM_SLOTS = 256

   !> Easing rates (1/s).
   real(wp), parameter :: UI_K_HOVER = 16.0_wp
   real(wp), parameter :: UI_K_PRESS = 26.0_wp
   real(wp), parameter :: UI_K_VALUE = 11.0_wp
   !> Seconds for one ripple to expand and fade.
   real(wp), parameter :: UI_RIPPLE_TIME = 0.42_wp

   type :: ui_anim_slot
      integer(ip) :: id = 0
      integer(ip) :: frame = -100000
      real(wp) :: hov = 0.0_wp, hov_t = 0.0_wp
      real(wp) :: prs = 0.0_wp, prs_t = 0.0_wp
      real(wp) :: val = 0.0_wp, val_t = 0.0_wp
      real(wp) :: rip = 1.0_wp
   end type ui_anim_slot

   type :: ui_anim_store
      type(ui_anim_slot) :: s(ANIM_SLOTS)
      integer(ip) :: frame = 0
      real(wp) :: dt = 0.016_wp
   contains
      procedure :: begin => an_begin
      procedure :: step => an_step
      procedure :: push => an_push
      procedure :: value => an_value
   end type ui_anim_store

contains

   !> Start a frame: everything pushed after this belongs to it.
   subroutine an_begin(self, dt)
      class(ui_anim_store), intent(inout) :: self
      real(wp), intent(in) :: dt

      self%frame = self%frame + 1
      self%dt = min(max(dt, 0.0_wp), 0.1_wp)
   end subroutine an_begin

   !> Integrate every live slot. Called once at the end of the frame.
   subroutine an_step(self)
      class(ui_anim_store), intent(inout) :: self
      real(wp) :: ah, ap, av
      integer :: i

      ah = 1.0_wp - exp(-self%dt*UI_K_HOVER)
      ap = 1.0_wp - exp(-self%dt*UI_K_PRESS)
      av = 1.0_wp - exp(-self%dt*UI_K_VALUE)

      do i = 1, ANIM_SLOTS
         if (self%s(i)%id == 0) cycle
         self%s(i)%hov = self%s(i)%hov + (self%s(i)%hov_t - self%s(i)%hov)*ah
         self%s(i)%prs = self%s(i)%prs + (self%s(i)%prs_t - self%s(i)%prs)*ap
         self%s(i)%val = self%s(i)%val + (self%s(i)%val_t - self%s(i)%val)*av
         if (self%s(i)%rip < 1.0_wp) then
            self%s(i)%rip = min(1.0_wp, self%s(i)%rip + self%dt/UI_RIPPLE_TIME)
         end if
         ! drop slots nobody touched for a while
         if (self%frame - self%s(i)%frame > 240) then
            self%s(i)%id = 0
            self%s(i)%hov = 0.0_wp
            self%s(i)%prs = 0.0_wp
            self%s(i)%val = 0.0_wp
            self%s(i)%rip = 1.0_wp
         end if
      end do
   end subroutine an_step

   integer function find_slot(self, id) result(k)
      class(ui_anim_store), intent(inout) :: self
      integer(ip), intent(in) :: id
      integer :: probe, start

      start = 1 + int(iand(id, int(ANIM_SLOTS - 1, ip)))
      do probe = 0, ANIM_SLOTS - 1
         k = 1 + mod(start - 1 + probe, ANIM_SLOTS)
         if (self%s(k)%id == id) return
         if (self%s(k)%id == 0) then
            self%s(k)%id = id
            self%s(k)%hov = 0.0_wp
            self%s(k)%hov_t = 0.0_wp
            self%s(k)%prs = 0.0_wp
            self%s(k)%prs_t = 0.0_wp
            self%s(k)%val = 0.0_wp
            self%s(k)%val_t = 0.0_wp
            self%s(k)%rip = 1.0_wp
            return
         end if
      end do
      ! table full: recycle the first slot
      k = start
      self%s(k) = ui_anim_slot()
      self%s(k)%id = id
   end function find_slot

   !> Report interaction state and read back the eased amounts.
   !> hov/press are 0..1; hov eases fast, press faster.
   subroutine an_push(self, id, hovered, pressed, hov, press)
      class(ui_anim_store), intent(inout) :: self
      integer(ip), intent(in) :: id
      logical, intent(in) :: hovered, pressed
      real(wp), intent(out), optional :: hov, press
      integer :: k

      k = find_slot(self, id)
      self%s(k)%frame = self%frame
      self%s(k)%hov_t = merge(1.0_wp, 0.0_wp, hovered)
      self%s(k)%prs_t = merge(1.0_wp, 0.0_wp, pressed)
      if (present(hov)) hov = self%s(k)%hov
      if (present(press)) press = self%s(k)%prs
   end subroutine an_push

   !> Drive a generic 0..1 value (switch position, progress fill, ...).
   subroutine an_value(self, id, target, cur)
      class(ui_anim_store), intent(inout) :: self
      integer(ip), intent(in) :: id
      real(wp), intent(in) :: target
      real(wp), intent(out) :: cur
      integer :: k

      k = find_slot(self, id)
      self%s(k)%frame = self%frame
      self%s(k)%val_t = target
      cur = self%s(k)%val
   end subroutine an_value

   !> Linear interpolation between two colours, alpha included.
   function ui_mix_color(a, b, t) result(c)
      type(ui_color), intent(in) :: a, b
      real(wp), intent(in) :: t
      type(ui_color) :: c
      real(wp) :: u, k

      u = min(max(t, 0.0_wp), 1.0_wp)
      k = 1.0_wp - u
      c%r = nint(real(a%r, wp)*k + real(b%r, wp)*u)
      c%g = nint(real(a%g, wp)*k + real(b%g, wp)*u)
      c%b = nint(real(a%b, wp)*k + real(b%b, wp)*u)
      c%a = nint(real(a%a, wp)*k + real(b%a, wp)*u)
   end function ui_mix_color

   !> Cubic ease-out, for anything driven by a 0..1 progress.
   pure function ui_ease_out(t) result(v)
      real(wp), intent(in) :: t
      real(wp) :: v, u

      u = min(max(t, 0.0_wp), 1.0_wp)
      v = 1.0_wp - (1.0_wp - u)**3
   end function ui_ease_out

end module ui_anim
