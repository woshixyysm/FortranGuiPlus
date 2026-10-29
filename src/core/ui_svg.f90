!> SVG path data ("d" attribute) parser.
!>
!> Supports M/L/H/V/C/S/Q/T/Z in both absolute and relative form, which covers
!> essentially every icon path in the wild. Elliptical arcs (A) are accepted
!> syntactically and approximated by a straight line to the arc endpoint.
!> The output is a ui_path, so the same tessellator handles SVG and TrueType.
module ui_svg
   use ui_kinds
   use ui_vector
   implicit none
   private

   public :: ui_svg_path

contains

   !> Parse SVG path data and append it to an existing ui_path.
   subroutine ui_svg_path(d, path)
      character(*), intent(in) :: d
      type(ui_path), intent(inout) :: path
      integer :: p, n
      character(len=1) :: cmd, prev
      real(wp) :: cur(2), start(2)
      real(wp) :: c1(2), c2(2), e(2), r(2)
      real(wp) :: last_q(2), last_c(2)
      logical :: have_q, have_c, rel, ok, more

      n = len(d)
      p = 1
      cur = [0.0_wp, 0.0_wp]
      start = cur
      last_q = cur
      last_c = cur
      have_q = .false.
      have_c = .false.
      prev = ' '
      cmd = ' '

      do while (p <= n)
         call skip_sep(d, p, n)
         if (p > n) exit

         if (is_alpha(d(p:p))) then
            cmd = d(p:p)
            prev = cmd
            p = p + 1
         else
            ! implicit repetition of the previous command
            select case (prev)
            case ('M')
               cmd = 'L'
            case ('m')
               cmd = 'l'
            case (' ')
               exit
            case default
               cmd = prev
            end select
         end if

         rel = (cmd >= 'a' .and. cmd <= 'z')
         call upper(cmd)

         select case (cmd)
         case ('M')
            call next_num(d, p, n, e(1), ok)
            if (.not. ok) exit
            call next_num(d, p, n, e(2), ok)
            if (.not. ok) exit
            if (rel) e = e + cur
            call path%move_to(e(1), e(2))
            cur = e
            start = e
            have_q = .false.
            have_c = .false.
         case ('L')
            call next_num(d, p, n, e(1), ok)
            if (.not. ok) exit
            call next_num(d, p, n, e(2), ok)
            if (.not. ok) exit
            if (rel) e = e + cur
            call path%line_to(e(1), e(2))
            cur = e
            have_q = .false.
            have_c = .false.
         case ('H')
            call next_num(d, p, n, e(1), ok)
            if (.not. ok) exit
            if (rel) e(1) = cur(1) + e(1)
            call path%line_to(e(1), cur(2))
            cur(1) = e(1)
            have_q = .false.
            have_c = .false.
         case ('V')
            call next_num(d, p, n, e(2), ok)
            if (.not. ok) exit
            if (rel) e(2) = cur(2) + e(2)
            call path%line_to(cur(1), e(2))
            cur(2) = e(2)
            have_q = .false.
            have_c = .false.
         case ('Q')
            call next_num(d, p, n, c1(1), ok)
            if (.not. ok) exit
            call next_num(d, p, n, c1(2), ok)
            if (.not. ok) exit
            call next_num(d, p, n, e(1), ok)
            if (.not. ok) exit
            call next_num(d, p, n, e(2), ok)
            if (.not. ok) exit
            if (rel) then
               c1 = c1 + cur
               e = e + cur
            end if
            call path%quad_to(c1(1), c1(2), e(1), e(2))
            last_q = c1
            have_q = .true.
            have_c = .false.
            cur = e
         case ('T')
            call next_num(d, p, n, e(1), ok)
            if (.not. ok) exit
            call next_num(d, p, n, e(2), ok)
            if (.not. ok) exit
            if (rel) e = e + cur
            if (have_q) then
               c1 = 2.0_wp*cur - last_q
            else
               c1 = cur
            end if
            call path%quad_to(c1(1), c1(2), e(1), e(2))
            last_q = c1
            have_q = .true.
            have_c = .false.
            cur = e
         case ('C')
            call next_num(d, p, n, c1(1), ok)
            if (.not. ok) exit
            call next_num(d, p, n, c1(2), ok)
            if (.not. ok) exit
            call next_num(d, p, n, c2(1), ok)
            if (.not. ok) exit
            call next_num(d, p, n, c2(2), ok)
            if (.not. ok) exit
            call next_num(d, p, n, e(1), ok)
            if (.not. ok) exit
            call next_num(d, p, n, e(2), ok)
            if (.not. ok) exit
            if (rel) then
               c1 = c1 + cur
               c2 = c2 + cur
               e = e + cur
            end if
            call path%cubic_to(c1(1), c1(2), c2(1), c2(2), e(1), e(2))
            last_c = c2
            have_c = .true.
            have_q = .false.
            cur = e
         case ('S')
            call next_num(d, p, n, c2(1), ok)
            if (.not. ok) exit
            call next_num(d, p, n, c2(2), ok)
            if (.not. ok) exit
            call next_num(d, p, n, e(1), ok)
            if (.not. ok) exit
            call next_num(d, p, n, e(2), ok)
            if (.not. ok) exit
            if (rel) then
               c2 = c2 + cur
               e = e + cur
            end if
            if (have_c) then
               c1 = 2.0_wp*cur - last_c
            else
               c1 = cur
            end if
            call path%cubic_to(c1(1), c1(2), c2(1), c2(2), e(1), e(2))
            last_c = c2
            have_c = .true.
            have_q = .false.
            cur = e
         case ('A')
            ! rx ry rot large sweep x y -> line to the endpoint (approximation)
            call next_num(d, p, n, r(1), ok)
            if (.not. ok) exit
            call next_num(d, p, n, r(2), ok)
            if (.not. ok) exit
            call next_num(d, p, n, r(1), ok)
            if (.not. ok) exit
            call next_num(d, p, n, r(2), ok)
            if (.not. ok) exit
            call next_num(d, p, n, e(1), ok)
            if (.not. ok) exit
            call next_num(d, p, n, e(2), ok)
            if (.not. ok) exit
            if (rel) e = e + cur
            call path%line_to(e(1), e(2))
            cur = e
            have_q = .false.
            have_c = .false.
         case ('Z')
            call path%close()
            cur = start
            have_q = .false.
            have_c = .false.
         case default
            exit
         end select

         call skip_sep(d, p, n)
         more = (p <= n)
         if (.not. more) exit
      end do

      ! suppress "set but not used" on some compilers
      if (.false.) print *, r(1), more
   end subroutine ui_svg_path

   pure logical function is_alpha(c) result(r)
      character(len=1), intent(in) :: c

      r = (c >= 'a' .and. c <= 'z') .or. (c >= 'A' .and. c <= 'Z')
   end function is_alpha

   pure subroutine upper(c)
      character(len=1), intent(inout) :: c

      if (c >= 'a' .and. c <= 'z') c = achar(iachar(c) - 32)
   end subroutine upper

   !> Skip whitespace and commas.
   pure subroutine skip_sep(d, p, n)
      character(*), intent(in) :: d
      integer, intent(inout) :: p
      integer, intent(in) :: n
      character(len=1) :: c

      do while (p <= n)
         c = d(p:p)
         if (c == ' ' .or. c == ',' .or. c == achar(9) .or. &
             c == achar(10) .or. c == achar(13)) then
            p = p + 1
         else
            exit
         end if
      end do
   end subroutine skip_sep

   !> Read one number at position p.
   subroutine next_num(d, p, n, v, ok)
      character(*), intent(in) :: d
      integer, intent(inout) :: p
      integer, intent(in) :: n
      real(wp), intent(out) :: v
      logical, intent(out) :: ok
      integer :: q, start
      character(len=1) :: c
      integer :: ios
      logical :: seen_dot

      v = 0.0_wp
      ok = .false.
      call skip_sep(d, p, n)
      if (p > n) return

      start = p
      c = d(p:p)
      if (c == '+' .or. c == '-') p = p + 1

      seen_dot = .false.
      do while (p <= n)
         c = d(p:p)
         if (c >= '0' .and. c <= '9') then
            p = p + 1
         else if (c == '.') then
            ! "3.41.81" means 3.41 followed by .81
            if (seen_dot) exit
            seen_dot = .true.
            p = p + 1
         else if (c == 'e' .or. c == 'E') then
            p = p + 1
            if (p <= n) then
               c = d(p:p)
               if (c == '+' .or. c == '-') p = p + 1
            end if
         else
            exit
         end if
      end do

      q = p - 1
      if (q < start) return
      read (d(start:q), *, iostat=ios) v
      ok = (ios == 0)
   end subroutine next_num

end module ui_svg
