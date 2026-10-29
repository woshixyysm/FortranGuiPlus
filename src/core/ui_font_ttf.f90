!> Minimal TrueType / OpenType (glyf) font loader.
!>
!> Parses head, maxp, hhea, hmtx, loca, glyf and cmap(format 4) directly with
!> Fortran stream I/O - no FreeType, no C. Simple glyphs are converted from
!> their quadratic off-curve points into a ui_path, so text becomes real
!> vector geometry at any size.
!>
!> Limits (documented, not accidental):
!>   * composite glyphs (numberOfContours = -1) are skipped
!>   * cmap format 12 (code points above U+FFFF) is not used
!>   * CFF/PostScript (.otf) outlines are not supported, use .ttf
module ui_font_ttf
   use ui_kinds
   use ui_vector
   implicit none
   private

   public :: ui_ttf, ui_ttf_load, ui_ttf_free
   public :: ui_ttf_glyph_index, ui_ttf_advance
   public :: ui_ttf_text_path, ui_ttf_text_width, ui_ttf_line_height
   public :: ui_ttf_glyph_debug

   type :: ui_ttf
      integer(int8), allocatable :: data(:)
      logical :: loaded = .false.
      integer :: units_per_em = 1000
      integer :: num_glyphs = 0
      integer :: num_hmetrics = 0
      integer :: loca_format = 0
      integer :: off_loca = 0
      integer :: off_glyf = 0
      integer :: off_hmtx = 0
      integer :: off_cmap = 0
      integer :: off_cmap_fmt = 0
      integer :: cmap_seg = 0
   contains
      procedure :: glyph_index => ttf_glyph_index
      procedure :: glyph_path => ttf_glyph_path
   end type ui_ttf

contains

   !------------------------------------------------------------------
   ! little endian readers on a 1-based byte string
   !------------------------------------------------------------------

   pure integer function u8(d, i) result(v)
      integer(int8), intent(in) :: d(:)
      integer, intent(in) :: i

      if (i < 1 .or. i > size(d)) then
         v = 0
      else
         v = iand(int(d(i), ip), 255)
      end if
   end function u8

   pure integer function u16(d, i) result(v)
      integer(int8), intent(in) :: d(:)
      integer, intent(in) :: i

      ! sfnt tables are big-endian
      v = 256*u8(d, i) + u8(d, i + 1)
   end function u16

   pure integer function i16(d, i) result(v)
      integer(int8), intent(in) :: d(:)
      integer, intent(in) :: i

      v = u16(d, i)
      if (v >= 32768) v = v - 65536
   end function i16

   pure integer function i32(d, i) result(v)
      integer(int8), intent(in) :: d(:)
      integer, intent(in) :: i

      v = int(iand(65536_i64*int(u16(d, i), i64) + int(u16(d, i + 2), i64), &
                  2147483647_i64), ip)
   end function i32

   pure character(len=4) function tag(d, i) result(t)
      integer(int8), intent(in) :: d(:)
      integer, intent(in) :: i
      integer :: k

      do k = 0, 3
         t(k + 1:k + 1) = achar(u8(d, i + k))
      end do
   end function tag

   !------------------------------------------------------------------
   ! loading
   !------------------------------------------------------------------

   !> Load a .ttf file. ierr is 0 on success.
   subroutine ui_ttf_load(font, path, ierr)
      type(ui_ttf), intent(inout) :: font
      character(*), intent(in) :: path
      integer, intent(out) :: ierr
      integer :: u, ios, n, ntab, i, off, tlen, toff, found_cmap, cp_off, cmap_off

      ierr = 0
      call ui_ttf_free(font)

      inquire (file=path, size=n)
      if (n <= 0) then
         ierr = 1
         return
      end if

      allocate (font%data(n))
      open (newunit=u, file=path, access='stream', form='unformatted', &
            status='old', action='read', iostat=ios)
      if (ios /= 0) then
         ierr = 2
         return
      end if
      read (u, iostat=ios) font%data
      close (u)
      if (ios /= 0) then
         ierr = 3
         return
      end if

      ntab = u16(font%data, 5)
      font%off_loca = 0
      font%off_glyf = 0
      font%off_hmtx = 0
      font%off_cmap = 0
      cmap_off = 0

      do i = 1, ntab
         off = 12 + (i - 1)*16 + 1
         if (off + 15 > n) exit
         toff = i32(font%data, off + 8)
         tlen = i32(font%data, off + 12)
         select case (tag(font%data, off))
         case ('head')
            font%units_per_em = u16(font%data, toff + 19)
            font%loca_format = i16(font%data, toff + 51)
         case ('maxp')
            font%num_glyphs = u16(font%data, toff + 5)
         case ('hhea')
            font%num_hmetrics = u16(font%data, toff + 35)
         case ('hmtx')
            font%off_hmtx = toff
         case ('loca')
            font%off_loca = toff
         case ('glyf')
            font%off_glyf = toff
         case ('cmap')
            cmap_off = toff
         end select
         if (tlen < 0) exit
      end do

      if (font%off_loca == 0) then
         ierr = 5
         return
      end if
      if (font%off_glyf == 0) then
         ierr = 6
         return
      end if
      if (font%units_per_em <= 0) then
         ierr = 7
         return
      end if

      ! cmap: prefer the Windows Unicode BMP subtable (3,1, format 4)
      found_cmap = 0
      if (cmap_off > 0) then
         n = u16(font%data, cmap_off + 3)          ! numTables in the cmap header
         do i = 1, n
            off = cmap_off + 4 + (i - 1)*8 + 1     ! encoding record, 1 based
            if (off + 7 > size(font%data)) exit
            if (u16(font%data, off) == 3 .and. u16(font%data, off + 2) == 1) then
               found_cmap = cmap_off + i32(font%data, off + 4) + 1
               exit
            end if
         end do
      end if
      if (found_cmap > 0 .and. u16(font%data, found_cmap) == 4) then
         font%off_cmap = found_cmap
         font%off_cmap_fmt = 4
         font%cmap_seg = u16(font%data, found_cmap + 6)/2
      end if

      font%loaded = .true.
   end subroutine ui_ttf_load

   subroutine ui_ttf_free(font)
      type(ui_ttf), intent(inout) :: font

      if (allocated(font%data)) deallocate (font%data)
      font%loaded = .false.
   end subroutine ui_ttf_free

   !------------------------------------------------------------------
   ! metrics
   !------------------------------------------------------------------

   !> Unicode code point -> glyph id (cmap format 4).
   function ttf_glyph_index(self, cp) result(gid)
      class(ui_ttf), intent(in) :: self
      integer, intent(in) :: cp
      integer :: gid, k, ec, sc, delta, ro, addr, p
      integer :: end_base, start_base, delta_base, ro_base

      gid = 0
      if (.not. self%loaded) return
      if (self%off_cmap == 0) then
         gid = cp
         return
      end if

      end_base = self%off_cmap + 14
      start_base = end_base + 2*self%cmap_seg + 2
      delta_base = start_base + 2*self%cmap_seg
      ro_base = delta_base + 2*self%cmap_seg

      do k = 0, self%cmap_seg - 1
         ec = u16(self%data, end_base + 2*k)
         if (cp > ec) cycle
         sc = u16(self%data, start_base + 2*k)
         if (cp < sc) exit
         delta = i16(self%data, delta_base + 2*k)
         ro = u16(self%data, ro_base + 2*k)
         if (ro == 0) then
            gid = iand(cp + delta, 65535)
         else
            addr = ro_base + 2*k + ro + 2*(cp - sc)
            p = u16(self%data, addr)
            if (p /= 0) gid = iand(p + delta, 65535)
         end if
         return
      end do
   end function ttf_glyph_index

   !> Glyph index for a character (wrapper).
   function ui_ttf_glyph_index(font, ch) result(gid)
      type(ui_ttf), intent(in) :: font
      character(len=1), intent(in) :: ch
      integer :: gid

      gid = font%glyph_index(iachar(ch))
   end function ui_ttf_glyph_index

   !> Advance width of a glyph in em units.
   function ui_ttf_advance(font, gid) result(adv)
      type(ui_ttf), intent(in) :: font
      integer, intent(in) :: gid
      real(wp) :: adv
      integer :: i

      adv = 0.0_wp
      if (.not. font%loaded .or. font%off_hmtx == 0) return
      if (font%num_hmetrics <= 0) return
      i = min(max(gid, 0), font%num_hmetrics - 1)
      adv = real(u16(font%data, font%off_hmtx + 4*i + 1), wp)
   end function ui_ttf_advance

   pure real(wp) function ui_ttf_line_height(font) result(h)
      type(ui_ttf), intent(in) :: font

      h = 1.2_wp
   end function ui_ttf_line_height

   !------------------------------------------------------------------
   ! outlines
   !------------------------------------------------------------------

   !> Append one glyph outline, scaled to `size` pixels and placed with its
   !> pen origin at (pen_x, baseline_y).
   subroutine ttf_glyph_path(self, gid, size, pen_x, baseline, path)
      class(ui_ttf), intent(in) :: self
      integer, intent(in) :: gid
      real(wp), intent(in) :: size, pen_x, baseline
      type(ui_path), intent(inout) :: path

      integer :: p0, ncont, i, k, off, ilen, ptot
      integer, allocatable :: endpts(:), flags(:), xs(:), ys(:)
      real(wp) :: sc

      if (.not. self%loaded) return
      if (gid < 0 .or. gid >= self%num_glyphs) return

      if (self%loca_format == 0) then
         off = self%off_glyf + 2*u16(self%data, self%off_loca + 2*gid + 1)
         p0 = self%off_glyf + 2*u16(self%data, self%off_loca + 2*(gid + 1) + 1)
      else
         off = self%off_glyf + i32(self%data, self%off_loca + 4*gid + 1)
         p0 = self%off_glyf + i32(self%data, self%off_loca + 4*(gid + 1) + 1)
      end if
      if (p0 <= off) return          ! empty glyph, e.g. space

      ncont = i16(self%data, off + 1)
      if (ncont <= 0) return         ! composite: not supported

      allocate (endpts(ncont))
      do i = 1, ncont
         endpts(i) = u16(self%data, off + 11 + 2*(i - 1))
      end do
      ptot = endpts(ncont) + 1

      ilen = u16(self%data, off + 11 + 2*ncont)
      p0 = off + 13 + 2*ncont + ilen

      allocate (flags(ptot), xs(ptot), ys(ptot))

      i = 1
      do while (i <= ptot)
         flags(i) = u8(self%data, p0)
         p0 = p0 + 1
         if (btest(flags(i), 3)) then
            k = u8(self%data, p0)
            p0 = p0 + 1
            do
               if (i >= ptot) exit
               i = i + 1
               flags(i) = flags(i - 1)
               k = k - 1
               if (k <= 0) exit
            end do
         end if
         i = i + 1
      end do

      ! x coordinates
      k = 0
      i = 1
      do while (i <= ptot)
         if (btest(flags(i), 1)) then
            k = k + u8(self%data, p0)
            p0 = p0 + 1
            if (.not. btest(flags(i), 4)) k = k - 256
         else if (.not. btest(flags(i), 4)) then
            k = k + i16(self%data, p0)
            p0 = p0 + 2
         end if
         xs(i) = k
         i = i + 1
      end do

      ! y coordinates
      k = 0
      i = 1
      do while (i <= ptot)
         if (btest(flags(i), 2)) then
            k = k + u8(self%data, p0)
            p0 = p0 + 1
            if (.not. btest(flags(i), 5)) k = k - 256
         else if (.not. btest(flags(i), 5)) then
            k = k + i16(self%data, p0)
            p0 = p0 + 2
         end if
         ys(i) = k
         i = i + 1
      end do

      sc = size/real(max(self%units_per_em, 1), wp)
      call emit_contours(path, ncont, endpts, flags, xs, ys, &
                         sc, pen_x, baseline)

      deallocate (endpts, flags, xs, ys)
   end subroutine ttf_glyph_path

   !> Walk the point list of every contour and emit lines / quadratic curves.
   subroutine emit_contours(path, ncont, endpts, flags, xs, ys, sc, px, base)
      type(ui_path), intent(inout) :: path
      integer, intent(in) :: ncont, endpts(:), flags(:), xs(:), ys(:)
      real(wp), intent(in) :: sc, px, base
      integer :: c, a, b, n, i, j, s, t
      real(wp) :: vx(1:endpts(ncont) + 1), vy(1:endpts(ncont) + 1)
      logical :: von(1:endpts(ncont) + 1)
      real(wp) :: sx, sy, mx, my

      a = 1
      do c = 1, ncont
         b = endpts(c) + 1
         n = b - a + 1
         if (n >= 2) then

            ! find the first on-curve point of the contour
            s = -1
            do i = 0, n - 1
               if (btest(flags(a + i), 0)) then
                  s = i
                  exit
               end if
            end do

            if (s >= 0) then
               do i = 0, n - 1
                  j = a + mod(s + i, n)
                  vx(i + 1) = real(xs(j), wp)*sc + px
                  vy(i + 1) = base - real(ys(j), wp)*sc
                  von(i + 1) = btest(flags(j), 0)
               end do
               sx = vx(1)
               sy = vy(1)
               t = 2
            else
               do i = 0, n - 1
                  j = a + i
                  vx(i + 1) = real(xs(j), wp)*sc + px
                  vy(i + 1) = base - real(ys(j), wp)*sc
                  von(i + 1) = .false.
               end do
               sx = 0.5_wp*(vx(n) + vx(1))
               sy = 0.5_wp*(vy(n) + vy(1))
               t = 1
            end if

            call path%move_to(sx, sy)

            do while (t <= n)
               if (von(t)) then
                  call path%line_to(vx(t), vy(t))
                  t = t + 1
               else if (t == n) then
                  call path%quad_to(vx(t), vy(t), sx, sy)
                  t = t + 1
               else if (von(t + 1)) then
                  call path%quad_to(vx(t), vy(t), vx(t + 1), vy(t + 1))
                  t = t + 2
               else
                  mx = 0.5_wp*(vx(t) + vx(t + 1))
                  my = 0.5_wp*(vy(t) + vy(t + 1))
                  call path%quad_to(vx(t), vy(t), mx, my)
                  t = t + 1
               end if
            end do

            call path%close()
         end if
         a = b + 1
      end do
   end subroutine emit_contours

   !> Diagnostic: dump the raw decoded points of one glyph.
   subroutine ui_ttf_glyph_debug(font, gid)
      type(ui_ttf), intent(in) :: font
      integer, intent(in) :: gid
      integer :: off, ncont, ptot, i, p0, ilen, k
      integer, allocatable :: endpts(:), flags(:), xs(:), ys(:)

      write (*, *) 'DBG glyph ', gid, ' upem=', font%units_per_em
      if (font%loca_format == 0) then
         off = font%off_glyf + 2*u16(font%data, font%off_loca + 2*gid + 1)
      else
         off = font%off_glyf + i32(font%data, font%off_loca + 4*gid + 1)
      end if
      ncont = i16(font%data, off + 1)
      write (*, *) 'DBG ncont=', ncont, ' off=', off
      if (ncont <= 0) return
      allocate (endpts(ncont))
      do i = 1, ncont
         endpts(i) = u16(font%data, off + 11 + 2*(i - 1))
      end do
      ptot = endpts(ncont) + 1
      ilen = u16(font%data, off + 11 + 2*ncont)
      p0 = off + 13 + 2*ncont + ilen
      write (*, *) 'DBG ptot=', ptot, ' ilen=', ilen, ' flags@', p0
      allocate (flags(ptot), xs(ptot), ys(ptot))
      i = 1
      do while (i <= ptot)
         flags(i) = u8(font%data, p0)
         p0 = p0 + 1
         if (btest(flags(i), 3)) then
            k = u8(font%data, p0)
            p0 = p0 + 1
            do
               if (i >= ptot) exit
               i = i + 1
               flags(i) = flags(i - 1)
               k = k - 1
               if (k <= 0) exit
            end do
         end if
         i = i + 1
      end do
      k = 0
      do i = 1, ptot
         if (btest(flags(i), 1)) then
            k = k + u8(font%data, p0)
            p0 = p0 + 1
            if (.not. btest(flags(i), 4)) k = k - 256
         else if (.not. btest(flags(i), 4)) then
            k = k + i16(font%data, p0)
            p0 = p0 + 2
         end if
         xs(i) = k
      end do
      k = 0
      do i = 1, ptot
         if (btest(flags(i), 2)) then
            k = k + u8(font%data, p0)
            p0 = p0 + 1
            if (.not. btest(flags(i), 5)) k = k - 256
         else if (.not. btest(flags(i), 5)) then
            k = k + i16(font%data, p0)
            p0 = p0 + 2
         end if
         ys(i) = k
      end do
      do i = 1, min(ptot, 14)
         write (*, '(a,i3,a,i6,a,i6,a,i3)') 'DBG pt', i, ' x=', xs(i), ' y=', &
            ys(i), ' fl=', flags(i)
      end do
   end subroutine ui_ttf_glyph_debug

   !> Lay out a string as vector outlines.
   subroutine ui_ttf_text_path(font, text, size, x, baseline, path)
      type(ui_ttf), intent(in) :: font
      character(*), intent(in) :: text
      real(wp), intent(in) :: size, x, baseline
      type(ui_path), intent(inout) :: path
      integer :: i, gid
      real(wp) :: pen

      pen = x
      do i = 1, len_trim(text)
         gid = font%glyph_index(iachar(text(i:i)))
         call font%glyph_path(gid, size, pen, baseline, path)
         pen = pen + ui_ttf_advance(font, gid)*size/ &
               real(max(font%units_per_em, 1), wp)
      end do
   end subroutine ui_ttf_text_path

   !> Width of a string in pixels.
   function ui_ttf_text_width(font, text, size) result(w)
      type(ui_ttf), intent(in) :: font
      character(*), intent(in) :: text
      real(wp), intent(in) :: size
      real(wp) :: w
      integer :: i, gid

      w = 0.0_wp
      do i = 1, len_trim(text)
         gid = font%glyph_index(iachar(text(i:i)))
         w = w + ui_ttf_advance(font, gid)*size/ &
             real(max(font%units_per_em, 1), wp)
      end do
   end function ui_ttf_text_width

end module ui_font_ttf
