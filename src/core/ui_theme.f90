!> Design-language theme layer: MD3 / MIUIX / WinUI Mica.
!>
!> A theme is pure data - colours + shape metrics. Widgets read it through
!> ui_style, so switching look is one call:
!>
!>   type(ui_theme_t) :: t
!>   call app%style = theme_md3_light()
!>   ! or, with Monet:
!>   call app%style = theme_md3_from_seed(seed, dark=.true.)
!>
!> Everything is drawn with the same primitives (rect / round_rect / circle /
!> gradient / glow), so the software and GL backends need no changes.
module ui_theme
   use ui_kinds
   use ui_color_hct
   implicit none
   private

   public :: ui_theme_t
   public :: theme_md3_light, theme_md3_dark, theme_md3_from_seed
   public :: theme_md3_expressive
   public :: theme_miuix, theme_winui_mica

   !> One complete look.
   type :: ui_theme_t
      character(len=24) :: name = 'custom'
      ! surface family
      type(ui_color) :: surface = ui_color(30_ip, 32_ip, 38_ip, 255_ip)
      type(ui_color) :: surface_low = ui_color(38_ip, 41_ip, 50_ip, 255_ip)
      type(ui_color) :: surface_high = ui_color(52_ip, 56_ip, 66_ip, 255_ip)
      type(ui_color) :: surface_top = ui_color(70_ip, 76_ip, 92_ip, 210_ip)
      type(ui_color) :: surface_bot = ui_color(38_ip, 41_ip, 50_ip, 235_ip)
      ! content
      type(ui_color) :: on_surface = ui_color(232_ip, 236_ip, 244_ip, 255_ip)
      type(ui_color) :: on_surface_dim = ui_color(150_ip, 156_ip, 170_ip, 255_ip)
      ! accent family
      type(ui_color) :: primary = ui_color(86_ip, 156_ip, 214_ip, 255_ip)
      type(ui_color) :: on_primary = ui_color(12_ip, 20_ip, 30_ip, 255_ip)
      type(ui_color) :: primary_container = ui_color(38_ip, 62_ip, 88_ip, 255_ip)
      type(ui_color) :: on_primary_container = ui_color(206_ip, 228_ip, 250_ip, 255_ip)
      type(ui_color) :: outline = ui_color(72_ip, 78_ip, 92_ip, 255_ip)
      type(ui_color) :: outline_soft = ui_color(255_ip, 255_ip, 255_ip, 28_ip)
      ! shape metrics
      real(wp) :: radius_panel = 16.0_wp
      real(wp) :: radius_widget = 8.0_wp
      real(wp) :: radius_button = 8.0_wp
      real(wp) :: border_w = 0.0_wp
      real(wp) :: pad = 14.0_wp
      real(wp) :: spacing = 10.0_wp
      real(wp) :: glow_blur = 0.0_wp
      real(wp) :: glow_exposure = 1.0_wp
      type(ui_color) :: glow_color = ui_color(255_ip, 236_ip, 244_ip, 0_ip)
      logical :: pill = .false.
      logical :: gradient_surface = .true.
      !> Draw the panel as frosted glass over the blurred backdrop, and how
      !> strongly it is tinted (alpha = tint amount).
      logical :: frosted = .true.
      type(ui_color) :: frost_tint = ui_color(24_ip, 26_ip, 34_ip, 150_ip)
      !> Rim highlight + specular sweep (liquid glass).
      logical :: liquid = .true.
   end type ui_theme_t

contains

   !--- Material 3 ----------------------------------------------------

   function theme_md3_light() result(t)
      type(ui_theme_t) :: t

      t%name = 'MD3 light'
      t%surface = ui_color(254_ip, 247_ip, 255_ip, 250_ip)
      t%surface_low = ui_color(247_ip, 242_ip, 250_ip, 250_ip)
      t%surface_high = ui_color(236_ip, 230_ip, 240_ip, 250_ip)
      t%surface_top = ui_color(255_ip, 255_ip, 255_ip, 235_ip)
      t%surface_bot = ui_color(240_ip, 234_ip, 244_ip, 245_ip)
      t%on_surface = ui_color(29_ip, 27_ip, 32_ip, 255_ip)
      t%on_surface_dim = ui_color(73_ip, 69_ip, 79_ip, 255_ip)
      t%primary = ui_color(103_ip, 80_ip, 164_ip, 255_ip)
      t%on_primary = ui_color(255_ip, 255_ip, 255_ip, 255_ip)
      t%primary_container = ui_color(234_ip, 221_ip, 255_ip, 255_ip)
      t%on_primary_container = ui_color(33_ip, 0_ip, 93_ip, 255_ip)
      t%outline = ui_color(121_ip, 116_ip, 126_ip, 255_ip)
      t%outline_soft = ui_color(0_ip, 0_ip, 0_ip, 22_ip)
      ! MD3: buttons are full pills, cards 28dp
      t%radius_panel = 28.0_wp
      t%radius_widget = 12.0_wp
      t%radius_button = 20.0_wp
      t%border_w = 0.0_wp
      t%pad = 16.0_wp
      t%spacing = 10.0_wp
      t%glow_blur = 10.0_wp
      t%glow_exposure = 0.18_wp
      t%glow_color = ui_color(103_ip, 80_ip, 164_ip, 90_ip)
      t%pill = .true.
      t%gradient_surface = .false.
      t%frosted = .false.
   end function theme_md3_light

   function theme_md3_dark() result(t)
      type(ui_theme_t) :: t

      t%name = 'MD3 dark'
      t%surface = ui_color(20_ip, 18_ip, 24_ip, 250_ip)
      t%surface_low = ui_color(29_ip, 27_ip, 32_ip, 250_ip)
      t%surface_high = ui_color(43_ip, 41_ip, 48_ip, 250_ip)
      t%surface_top = ui_color(60_ip, 56_ip, 70_ip, 235_ip)
      t%surface_bot = ui_color(26_ip, 24_ip, 30_ip, 245_ip)
      t%on_surface = ui_color(230_ip, 224_ip, 233_ip, 255_ip)
      t%on_surface_dim = ui_color(202_ip, 196_ip, 208_ip, 255_ip)
      t%primary = ui_color(208_ip, 188_ip, 255_ip, 255_ip)
      t%on_primary = ui_color(56_ip, 30_ip, 114_ip, 255_ip)
      t%primary_container = ui_color(79_ip, 55_ip, 139_ip, 255_ip)
      t%on_primary_container = ui_color(234_ip, 221_ip, 255_ip, 255_ip)
      t%outline = ui_color(147_ip, 143_ip, 153_ip, 255_ip)
      t%outline_soft = ui_color(255_ip, 255_ip, 255_ip, 26_ip)
      t%radius_panel = 28.0_wp
      t%radius_widget = 12.0_wp
      t%radius_button = 20.0_wp
      t%border_w = 0.0_wp
      t%pad = 16.0_wp
      t%spacing = 10.0_wp
      t%glow_blur = 12.0_wp
      t%glow_exposure = 0.30_wp
      t%glow_color = ui_color(208_ip, 188_ip, 255_ip, 100_ip)
      t%pill = .true.
      t%gradient_surface = .false.
      t%frosted = .true.
      t%frost_tint = ui_color(18_ip, 16_ip, 22_ip, 170_ip)
   end function theme_md3_dark

   !> Monet: full MD3 role set generated from a seed colour.
   function theme_md3_from_seed(seed, dark) result(t)
      type(ui_color), intent(in) :: seed
      logical, intent(in), optional :: dark
      type(ui_theme_t) :: t
      type(ui_scheme) :: s
      logical :: d

      d = .false.
      if (present(dark)) d = dark
      s = ui_scheme_from_seed(seed, d)

      if (d) then
         t = theme_md3_dark()
      else
         t = theme_md3_light()
      end if
      t%name = 'MD3 monet'
      t%primary = s%primary
      t%on_primary = s%on_primary
      t%primary_container = s%primary_container
      t%on_primary_container = s%on_primary_container
      t%surface = s%surface
      t%surface_low = s%surface_container_low
      t%surface_high = s%surface_container_high
      t%surface_top = s%surface_container_high
      t%surface_bot = s%surface_container_low
      t%on_surface = s%on_surface
      t%on_surface_dim = s%on_surface_variant
      t%outline = s%outline
      t%glow_color = ui_color(s%primary%r, s%primary%g, s%primary%b, 90_ip)
   end function theme_md3_from_seed

   !> Material 3 Expressive: the 2025 language - bigger radii, more
   !> saturated containers, heavier type scale, generous spacing.
   function theme_md3_expressive() result(t)
      type(ui_theme_t) :: t

      t%name = 'MD3 expressive'
      t%surface = ui_color(16_ip, 14_ip, 22_ip, 250_ip)
      t%surface_low = ui_color(27_ip, 24_ip, 36_ip, 250_ip)
      t%surface_high = ui_color(44_ip, 40_ip, 58_ip, 250_ip)
      t%surface_top = ui_color(56_ip, 50_ip, 78_ip, 200_ip)
      t%surface_bot = ui_color(22_ip, 19_ip, 30_ip, 235_ip)
      t%on_surface = ui_color(240_ip, 234_ip, 248_ip, 255_ip)
      t%on_surface_dim = ui_color(196_ip, 188_ip, 210_ip, 255_ip)
      ! expressive: a saturated violet accent with warm secondary
      t%primary = ui_color(199_ip, 168_ip, 255_ip, 255_ip)
      t%on_primary = ui_color(56_ip, 20_ip, 110_ip, 255_ip)
      t%primary_container = ui_color(96_ip, 62_ip, 168_ip, 255_ip)
      t%on_primary_container = ui_color(240_ip, 230_ip, 255_ip, 255_ip)
      t%outline = ui_color(140_ip, 132_ip, 156_ip, 255_ip)
      t%outline_soft = ui_color(255_ip, 255_ip, 255_ip, 40_ip)
      ! the expressive shape scale: everything is rounder and chunkier
      t%radius_panel = 32.0_wp
      t%radius_widget = 20.0_wp
      t%radius_button = 24.0_wp
      t%border_w = 0.0_wp
      t%pad = 20.0_wp
      t%spacing = 14.0_wp
      t%glow_blur = 16.0_wp
      t%glow_exposure = 0.34_wp
      t%glow_color = ui_color(199_ip, 168_ip, 255_ip, 110_ip)
      t%pill = .true.
      t%gradient_surface = .true.
      t%frosted = .true.
      t%frost_tint = ui_color(26_ip, 18_ip, 44_ip, 176_ip)
   end function theme_md3_expressive

   !--- MIUIX / HyperOS -----------------------------------------------
   function theme_miuix() result(t)
      type(ui_theme_t) :: t
      type(ui_color) :: accent

      accent = ui_color(52_ip, 130_ip, 246_ip, 255_ip)
      t%name = 'MIUIX'
      ! HyperOS: near-white cards, very large radii, no visible borders
      t%surface = ui_color(255_ip, 255_ip, 255_ip, 248_ip)
      t%surface_low = ui_color(244_ip, 245_ip, 247_ip, 250_ip)
      t%surface_high = ui_color(233_ip, 235_ip, 239_ip, 250_ip)
      t%surface_top = ui_color(255_ip, 255_ip, 255_ip, 245_ip)
      t%surface_bot = ui_color(238_ip, 240_ip, 244_ip, 250_ip)
      t%on_surface = ui_color(24_ip, 24_ip, 26_ip, 255_ip)
      t%on_surface_dim = ui_color(122_ip, 126_ip, 134_ip, 255_ip)
      t%primary = accent
      t%on_primary = ui_color(255_ip, 255_ip, 255_ip, 255_ip)
      t%primary_container = ui_color(224_ip, 236_ip, 255_ip, 255_ip)
      t%on_primary_container = ui_color(20_ip, 66_ip, 140_ip, 255_ip)
      t%outline = ui_color(226_ip, 228_ip, 232_ip, 255_ip)
      t%outline_soft = ui_color(0_ip, 0_ip, 0_ip, 16_ip)
      t%radius_panel = 24.0_wp
      t%radius_widget = 18.0_wp
      t%radius_button = 22.0_wp
      t%border_w = 0.0_wp
      t%pad = 18.0_wp
      t%spacing = 12.0_wp
      t%glow_blur = 14.0_wp
      t%glow_exposure = 0.20_wp
      t%glow_color = ui_color(52_ip, 130_ip, 246_ip, 80_ip)
      t%pill = .true.
      t%gradient_surface = .true.
      t%frosted = .false.
   end function theme_miuix

   !--- WinUI 3 / Fluent, Mica backdrop -------------------------------

   function theme_winui_mica() result(t)
      type(ui_theme_t) :: t

      t%name = 'WinUI mica'
      ! Mica: a desaturated layer tinted by the desktop, low contrast
      t%surface = ui_color(32_ip, 32_ip, 32_ip, 230_ip)
      t%surface_low = ui_color(43_ip, 43_ip, 43_ip, 235_ip)
      t%surface_high = ui_color(58_ip, 58_ip, 58_ip, 240_ip)
      t%surface_top = ui_color(58_ip, 58_ip, 58_ip, 220_ip)
      t%surface_bot = ui_color(28_ip, 28_ip, 28_ip, 240_ip)
      t%on_surface = ui_color(255_ip, 255_ip, 255_ip, 255_ip)
      t%on_surface_dim = ui_color(190_ip, 190_ip, 190_ip, 255_ip)
      t%primary = ui_color(96_ip, 205_ip, 255_ip, 255_ip)
      t%on_primary = ui_color(0_ip, 24_ip, 36_ip, 255_ip)
      t%primary_container = ui_color(0_ip, 60_ip, 90_ip, 255_ip)
      t%on_primary_container = ui_color(200_ip, 235_ip, 255_ip, 255_ip)
      t%outline = ui_color(90_ip, 90_ip, 90_ip, 255_ip)
      t%outline_soft = ui_color(255_ip, 255_ip, 255_ip, 34_ip)
      ! Fluent: 8dp cards, 4dp controls, 1dp hairline border
      t%radius_panel = 8.0_wp
      t%radius_widget = 4.0_wp
      t%radius_button = 4.0_wp
      t%border_w = 1.0_wp
      t%pad = 16.0_wp
      t%spacing = 8.0_wp
      t%glow_blur = 0.0_wp
      t%glow_exposure = 0.0_wp
      t%glow_color = ui_color(96_ip, 205_ip, 255_ip, 0_ip)
      t%pill = .false.
      t%gradient_surface = .true.
      t%frosted = .true.
      t%frost_tint = ui_color(20_ip, 20_ip, 20_ip, 205_ip)
   end function theme_winui_mica

end module ui_theme
