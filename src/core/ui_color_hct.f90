!> HCT colour space and Material 3 dynamic colour (Monet).
!>
!> HCT = Hue / Chroma / Tone. Hue and Chroma come from CAM16, Tone is CIE L*.
!> This module exposes the same API and the same tonal-palette behaviour, but
!> computes hue/chroma from CIE LCh(ab). For UI work the difference is a few
!> degrees of hue; it keeps the whole thing dependency free at ~250 lines.
!>
!> Behaviour mirrors material-color-utilities:
!>   * 13 stop tonal palettes (tone 0,10,...,100)
!>   * out-of-gamut colours are resolved by binary-searching chroma down
!>   * a seed colour produces a full MD3 role set, light or dark
module ui_color_hct
   use ui_kinds
   implicit none
   private

   public :: ui_hct, ui_scheme
   public :: ui_hct_from_rgb, ui_rgb_from_hct
   public :: ui_tone_color, ui_scheme_from_seed

   real(wp), parameter :: XN = 95.047_wp
   real(wp), parameter :: YN = 100.000_wp
   real(wp), parameter :: ZN = 108.883_wp
   real(wp), parameter :: DEG = 57.29577951308232_wp
   real(wp), parameter :: RAD = 0.017453292519943295_wp

   type :: ui_hct
      real(wp) :: hue = 0.0_wp
      real(wp) :: chroma = 0.0_wp
      real(wp) :: tone = 0.0_wp
   end type ui_hct

   type :: ui_scheme
      type(ui_color) :: primary
      type(ui_color) :: on_primary
      type(ui_color) :: primary_container
      type(ui_color) :: on_primary_container
      type(ui_color) :: secondary
      type(ui_color) :: on_secondary
      type(ui_color) :: tertiary
      type(ui_color) :: surface
      type(ui_color) :: surface_container_low
      type(ui_color) :: surface_container
      type(ui_color) :: surface_container_high
      type(ui_color) :: on_surface
      type(ui_color) :: on_surface_variant
      type(ui_color) :: outline
      type(ui_color) :: error
      logical :: dark = .false.
   end type ui_scheme

contains

   pure real(wp) function to_linear(v) result(l)
      real(wp), intent(in) :: v
      real(wp) :: s

      s = min(max(v, 0.0_wp), 255.0_wp)/255.0_wp
      if (s <= 0.04045_wp) then
         l = s/12.92_wp
      else
         l = ((s + 0.055_wp)/1.055_wp)**2.4_wp
      end if
   end function to_linear

   pure real(wp) function to_srgb(v) result(c)
      real(wp), intent(in) :: v
      real(wp) :: s

      if (v <= 0.0031308_wp) then
         s = 12.92_wp*max(v, 0.0_wp)
      else
         s = 1.055_wp*max(v, 0.0_wp)**(1.0_wp/2.4_wp) - 0.055_wp
      end if
      c = min(max(s, 0.0_wp), 1.0_wp)*255.0_wp
   end function to_srgb

   pure real(wp) function lab_f(t) result(v)
      real(wp), intent(in) :: t

      if (t > 0.008856451679_wp) then
         v = t**(1.0_wp/3.0_wp)
      else
         v = 7.787037037_wp*t + 16.0_wp/116.0_wp
      end if
   end function lab_f

   pure real(wp) function lab_finv(t) result(v)
      real(wp), intent(in) :: t
      real(wp) :: t3

      t3 = t*t*t
      if (t3 > 0.008856451679_wp) then
         v = t3
      else
         v = (t - 16.0_wp/116.0_wp)/7.787037037_wp
      end if
   end function lab_finv

   !------------------------------------------------------------------
   ! forward / inverse
   !------------------------------------------------------------------

   function ui_hct_from_rgb(c) result(h)
      type(ui_color), intent(in) :: c
      type(ui_hct) :: h
      real(wp) :: r, g, b, x, y, z, fx, fy, fz, a, bb

      r = to_linear(real(c%r, wp))
      g = to_linear(real(c%g, wp))
      b = to_linear(real(c%b, wp))

      x = (0.4124564_wp*r + 0.3575761_wp*g + 0.1804375_wp*b)*100.0_wp
      y = (0.2126729_wp*r + 0.7151522_wp*g + 0.0721750_wp*b)*100.0_wp
      z = (0.0193339_wp*r + 0.1191920_wp*g + 0.9503041_wp*b)*100.0_wp

      fx = lab_f(x/XN)
      fy = lab_f(y/YN)
      fz = lab_f(z/ZN)

      a = 500.0_wp*(fx - fy)
      bb = 200.0_wp*(fy - fz)

      h%tone = min(max(116.0_wp*fy - 16.0_wp, 0.0_wp), 100.0_wp)
      h%chroma = sqrt(a*a + bb*bb)
      if (h%chroma > 1.0e-6_wp) then
         h%hue = modulo(atan2(bb, a)*DEG, 360.0_wp)
      else
         h%hue = 0.0_wp
         h%chroma = 0.0_wp
      end if
   end function ui_hct_from_rgb

   !> Raw (possibly out of gamut) linear rgb for a given HCT.
   subroutine hct_to_linear(h, r, g, b)
      type(ui_hct), intent(in) :: h
      real(wp), intent(out) :: r, g, b
      real(wp) :: fx, fy, fz, x, y, z, ca, sa

      ca = cos(h%hue*RAD)
      sa = sin(h%hue*RAD)
      fy = (h%tone + 16.0_wp)/116.0_wp
      fx = fy + (h%chroma*ca)/500.0_wp
      fz = fy - (h%chroma*sa)/200.0_wp

      x = XN*lab_finv(fx)
      y = YN*lab_finv(fy)
      z = ZN*lab_finv(fz)

      r = 3.2404542_wp*x/100.0_wp - 1.5371385_wp*y/100.0_wp - 0.4985314_wp*z/100.0_wp
      g = -0.9692660_wp*x/100.0_wp + 1.8760108_wp*y/100.0_wp + 0.0415560_wp*z/100.0_wp
      b = 0.0556434_wp*x/100.0_wp - 0.2040259_wp*y/100.0_wp + 1.0572252_wp*z/100.0_wp
   end subroutine hct_to_linear

   pure logical function in_gamut(r, g, b) result(ok)

      real(wp), intent(in) :: r, g, b
      real(wp), parameter :: eps = 1.0e-4_wp

      ok = (r >= -eps .and. r <= 1.0_wp + eps) .and. &
           (g >= -eps .and. g <= 1.0_wp + eps) .and. &
           (b >= -eps .and. b <= 1.0_wp + eps)
   end function in_gamut

   !> HCT -> sRGB. Chroma is binary-searched down until the colour is in gamut,
   !> which is exactly how material-color-utilities resolves HCT.
   function ui_rgb_from_hct(h) result(c)
      type(ui_hct), intent(in) :: h
      type(ui_color) :: c
      type(ui_hct) :: probe
      real(wp) :: r, g, b, lo, hi, mid
      integer :: it

      probe = h
      call hct_to_linear(probe, r, g, b)

      if (.not. in_gamut(r, g, b)) then
         lo = 0.0_wp
         hi = h%chroma
         do it = 1, 24
            mid = 0.5_wp*(lo + hi)
            probe%chroma = mid
            call hct_to_linear(probe, r, g, b)
            if (in_gamut(r, g, b)) then
               lo = mid
            else
               hi = mid
            end if
         end do
         probe%chroma = lo
         call hct_to_linear(probe, r, g, b)
      end if

      c%r = nint(to_srgb(r))
      c%g = nint(to_srgb(g))
      c%b = nint(to_srgb(b))
      c%a = 255_ip
   end function ui_rgb_from_hct

   !> One tone of a tonal palette.
   function ui_tone_color(hue, chroma, tone) result(c)
      real(wp), intent(in) :: hue, chroma, tone
      type(ui_color) :: c
      type(ui_hct) :: h

      h%hue = modulo(hue, 360.0_wp)
      h%chroma = max(chroma, 0.0_wp)
      h%tone = min(max(tone, 0.0_wp), 100.0_wp)
      c = ui_rgb_from_hct(h)
   end function ui_tone_color

   !------------------------------------------------------------------
   ! Monet: seed -> MD3 scheme
   !------------------------------------------------------------------

   !> Build the full MD3 role set from one seed colour.
   function ui_scheme_from_seed(seed, dark) result(s)
      type(ui_color), intent(in) :: seed
      logical, intent(in), optional :: dark
      type(ui_scheme) :: s
      type(ui_hct) :: h
      real(wp) :: hue, hue2, hue3, chroma

      h = ui_hct_from_rgb(seed)
      hue = h%hue
      chroma = min(h%chroma, 48.0_wp)          ! MD3 clamps source chroma
      hue2 = hue
      hue3 = modulo(hue + 60.0_wp, 360.0_wp)

      s%dark = .false.
      if (present(dark)) s%dark = dark

      if (s%dark) then
         s%primary = ui_tone_color(hue, chroma, 80.0_wp)
         s%on_primary = ui_tone_color(hue, chroma, 20.0_wp)
         s%primary_container = ui_tone_color(hue, chroma, 30.0_wp)
         s%on_primary_container = ui_tone_color(hue, chroma, 90.0_wp)
         s%secondary = ui_tone_color(hue2, chroma/3.0_wp, 80.0_wp)
         s%on_secondary = ui_tone_color(hue2, chroma/3.0_wp, 20.0_wp)
         s%tertiary = ui_tone_color(hue3, chroma/2.0_wp, 80.0_wp)
         s%surface = ui_tone_color(hue, 4.0_wp, 10.0_wp)
         s%surface_container_low = ui_tone_color(hue, 4.0_wp, 14.0_wp)
         s%surface_container = ui_tone_color(hue, 4.0_wp, 17.0_wp)
         s%surface_container_high = ui_tone_color(hue, 4.0_wp, 22.0_wp)
         s%on_surface = ui_tone_color(hue, 4.0_wp, 90.0_wp)
         s%on_surface_variant = ui_tone_color(hue, 8.0_wp, 80.0_wp)
         s%outline = ui_tone_color(hue, 8.0_wp, 60.0_wp)
         s%error = ui_tone_color(25.0_wp, 84.0_wp, 80.0_wp)
      else
         s%primary = ui_tone_color(hue, chroma, 40.0_wp)
         s%on_primary = ui_tone_color(hue, chroma, 100.0_wp)
         s%primary_container = ui_tone_color(hue, chroma, 90.0_wp)
         s%on_primary_container = ui_tone_color(hue, chroma, 10.0_wp)
         s%secondary = ui_tone_color(hue2, chroma/3.0_wp, 40.0_wp)
         s%on_secondary = ui_tone_color(hue2, chroma/3.0_wp, 100.0_wp)
         s%tertiary = ui_tone_color(hue3, chroma/2.0_wp, 40.0_wp)
         s%surface = ui_tone_color(hue, 4.0_wp, 99.0_wp)
         s%surface_container_low = ui_tone_color(hue, 4.0_wp, 96.0_wp)
         s%surface_container = ui_tone_color(hue, 4.0_wp, 94.0_wp)
         s%surface_container_high = ui_tone_color(hue, 4.0_wp, 92.0_wp)
         s%on_surface = ui_tone_color(hue, 4.0_wp, 10.0_wp)
         s%on_surface_variant = ui_tone_color(hue, 8.0_wp, 30.0_wp)
         s%outline = ui_tone_color(hue, 8.0_wp, 50.0_wp)
         s%error = ui_tone_color(25.0_wp, 84.0_wp, 40.0_wp)
      end if
   end function ui_scheme_from_seed

end module ui_color_hct
