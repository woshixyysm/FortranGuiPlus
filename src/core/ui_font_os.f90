!> Font outlines from the OS scaler, via shim/fui_font.c.
!>
!> Replaces the hand rolled 'glyf' decoder: the shim asks GDI for
!> GGO_NATIVE, which hands back the outline as TTPOLYGONHEADER +
!> TTPOLYCURVE - quadratic segments, composites already expanded,
!> code pages handled. We convert them straight into a ui_path.
!>
!> Only 4 entry points are bound; nothing else about the shim is exposed.
module ui_font_os
   use, intrinsic :: iso_c_binding
   use ui_kinds
   use ui_vector
   implicit none
   private

   public :: ui_font_open, ui_font_close, ui_font_advance_px
   public :: ui_font_text_path, ui_font_text_width, ui_font_ascent_ratio

   integer, parameter :: MAXSEG = 2048
   integer(int8), save :: seg_kind(MAXSEG)
   real(c_float), save :: seg_pts(4*MAXSEG)
   !> Pixel size the shim rasterises at; outlines are scaled from this.
   real(wp), save :: g_base = 34.0_wp

   interface
      function c_open(path, px) bind(c, name='fui_font_open') result(r)
         import :: c_char, c_int
         character(kind=c_char) :: path(*)
         integer(c_int), value :: px
         integer(c_int) :: r
      end function c_open

      subroutine c_close() bind(c, name='fui_font_close')
      end subroutine c_close

      function c_advance(cp) bind(c, name='fui_font_advance') result(r)
         import :: c_float, c_int
         integer(c_int), value :: cp
         real(c_float) :: r
      end function c_advance

      function c_glyph(cp, kinds, pts, cap) bind(c, name='fui_font_glyph') result(r)
         import :: c_int, c_float
         integer(c_int), value :: cp, cap
         integer(c_int) :: kinds(*)
         real(c_float) :: pts(*)
         integer(c_int) :: r
      end function c_glyph
   end interface

contains

   !> Load a font file at a pixel size. Returns .true. on success.
   logical function ui_font_open(path, px) result(ok)
      character(*), intent(in) :: path
      integer, intent(in) :: px
      integer(c_int) :: r

      r = c_open(cstr(path), int(px, c_int))
      ok = (r == 0)
      if (ok) g_base = real(max(px, 1), wp)
   end function ui_font_open

   subroutine ui_font_close()

      call c_close()
   end subroutine ui_font_close

   !> Advance width in pixels for one code point.
   function ui_font_advance_px(cp) result(w)
      integer, intent(in) :: cp
      real(wp) :: w

      w = real(c_advance(int(cp, c_int)), wp)
   end function ui_font_advance_px

   !> Append one glyph outline to a path, baseline at y, pen at x.
   subroutine ui_font_text_path(text, size, x, baseline, path)
      character(*), intent(in) :: text
      real(wp), intent(in) :: size, x, baseline
      type(ui_path), intent(inout) :: path
      integer :: i, n, k, nk(MAXSEG)
      real(wp) :: pen, q

      q = size/g_base
      pen = x
      do i = 1, len_trim(text)
         n = int(c_glyph(int(iachar(text(i:i)), c_int), nk, seg_pts, &
                         int(MAXSEG, c_int)))
         if (n > 0) then
            do k = 1, n
               if (nk(k) == 0) then
                  call path%move_to(pen + q*real(seg_pts(4*(k-1)+1), wp), &
                                    baseline - q*real(seg_pts(4*(k-1)+2), wp))
               else if (nk(k) == 1) then
                  call path%line_to(pen + q*real(seg_pts(4*(k-1)+3), wp), &
                                    baseline - q*real(seg_pts(4*(k-1)+4), wp))
               else
                  call path%quad_to(pen + q*real(seg_pts(4*(k-1)+1), wp), &
                                    baseline - q*real(seg_pts(4*(k-1)+2), wp), &
                                    pen + q*real(seg_pts(4*(k-1)+3), wp), &
                                    baseline - q*real(seg_pts(4*(k-1)+4), wp))
               end if
            end do
         end if
         pen = pen + q*ui_font_advance_px(iachar(text(i:i)))
      end do
      if (.false.) print *, size
   end subroutine ui_font_text_path

   !> Width of a string at an arbitrary pixel size.
   function ui_font_text_width(text, size) result(w)
      character(*), intent(in) :: text
      real(wp), intent(in), optional :: size
      real(wp) :: w, q
      integer :: i

      q = 1.0_wp
      if (present(size)) q = size/g_base
      w = 0.0_wp
      do i = 1, len_trim(text)
         w = w + q*ui_font_advance_px(iachar(text(i:i)))
      end do
   end function ui_font_text_width

   !> Ascent above the baseline as a fraction of the pixel size.
   pure function ui_font_ascent_ratio() result(r)
      real(wp) :: r

      r = 0.75_wp
   end function ui_font_ascent_ratio

   pure function cstr(s) result(c)
      character(*), intent(in) :: s
      character(len=len_trim(s) + 1, kind=c_char) :: c
      integer :: i

      do i = 1, len_trim(s)
         c(i:i) = s(i:i)
      end do
      c(len_trim(s) + 1:len_trim(s) + 1) = c_null_char
   end function cstr

end module ui_font_os
