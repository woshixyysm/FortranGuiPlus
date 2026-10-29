!> Built in 5x7 bitmap font (ASCII 32..126).
!> Each glyph is stored as 7 rows of 5 bits, packed as two hex digits per row.
!> Bit 4 is the leftmost pixel of a row.
module ui_font
   use, intrinsic :: iso_fortran_env, only: int32
   implicit none
   private

   public :: FONT_W, FONT_H, UI_FONT_ADVANCE
   public :: ui_font_init, ui_font_rows, ui_font_char_index

   !> Glyph cell size in pixels (unscaled).
   integer, parameter :: FONT_W = 5
   integer, parameter :: FONT_H = 7
   !> Horizontal advance in pixels (glyph width + 1 pixel gap).
   integer, parameter :: UI_FONT_ADVANCE = 6

   !> Number of glyphs covering ASCII 32..126.
   integer, parameter :: FONT_COUNT = 95

   !> 7 rows x 2 hex digits per glyph.
   character(len=14), parameter :: FONT_HEX(FONT_COUNT) = [ character(len=14) :: &
      '00000000000000', & ! ' '
      '04040404000400', & ! '!'
      '0A0A0000000000', & ! '"'
      '0A0A1F0A1F0A00', & ! '#'
      '040F140E051E04', & ! '$'
      '19190204081313', & ! '%'
      '0C12140815120D', & ! '&'
      '04040000000000', & ! '''
      '02040808080402', & ! '('
      '08040202020408', & ! ')'
      '0004150E150400', & ! '*'
      '0004041F040400', & ! '+'
      '00000000040408', & ! ','
      '0000001F000000', & ! '-'
      '00000000000400', & ! '.'
      '01020204080810', & ! '/'
      '0E11131519110E', & ! '0'
      '040C040404040E', & ! '1'
      '0E11010204081F', & ! '2'
      '1F02040201110E', & ! '3'
      '02060A121F0202', & ! '4'
      '1F101E0101110E', & ! '5'
      '0608101E11110E', & ! '6'
      '1F010204080808', & ! '7'
      '0E11110E11110E', & ! '8'
      '0E11110F01020C', & ! '9'
      '00040000040000', & ! ':'
      '00040000040408', & ! ';'
      '02040810080402', & ! '<'
      '00001F001F0000', & ! '='
      '08040201020408', & ! '>'
      '0E110102040004', & ! '?'
      '0E11171517100E', & ! '@'
      '040A111F111111', & ! 'A'
      '1E11111E11111E', & ! 'B'
      '0E11101010110E', & ! 'C'
      '1C12111111121C', & ! 'D'
      '1F10101E10101F', & ! 'E'
      '1F10101E101010', & ! 'F'
      '0E11101711110E', & ! 'G'
      '1111111F111111', & ! 'H'
      '0E04040404040E', & ! 'I'
      '0702020202120C', & ! 'J'
      '11121418141211', & ! 'K'
      '1010101010101F', & ! 'L'
      '111B1515111111', & ! 'M'
      '11191513111111', & ! 'N'
      '0E11111111110E', & ! 'O'
      '1E11111E101010', & ! 'P'
      '0E11111115120D', & ! 'Q'
      '1E11111E141211', & ! 'R'
      '0F10100E01011E', & ! 'S'
      '1F040404040404', & ! 'T'
      '1111111111110E', & ! 'U'
      '11111111110A04', & ! 'V'
      '11111115151B11', & ! 'W'
      '11110A040A1111', & ! 'X'
      '11110A04040404', & ! 'Y'
      '1F01020408101F', & ! 'Z'
      '0E08080808080E', & ! '['
      '10080804020201', & ! '\'
      '0E02020202020E', & ! ']'
      '040A1100000000', & ! '^'
      '0000000000001F', & ! '_'
      '08040000000000', & ! '`'
      '00000E010F110F', & ! 'a'
      '10101E1111111E', & ! 'b'
      '00000E1010100E', & ! 'c'
      '01010F1111110F', & ! 'd'
      '00000E111F100E', & ! 'e'
      '0604040E040404', & ! 'f'
      '000F11110F010E', & ! 'g'
      '10101E11111111', & ! 'h'
      '04000C0404040E', & ! 'i'
      '0200020202120C', & ! 'j'
      '10101214181412', & ! 'k'
      '0C04040404040E', & ! 'l'
      '00001A15151111', & ! 'm'
      '00001E11111111', & ! 'n'
      '00000E1111110E', & ! 'o'
      '00001E11111E10', & ! 'p'
      '00000F11110F01', & ! 'q'
      '00001619101010', & ! 'r'
      '00000F100E011E', & ! 's'
      '08081E08080906', & ! 't'
      '0000111111110F', & ! 'u'
      '00001111110A04', & ! 'v'
      '0000111115150A', & ! 'w'
      '0000110A040A11', & ! 'x'
      '00001111110F01', & ! 'y'
      '00001F0204081F', & ! 'z'
      '03040408040403', & ! '{'
      '04040404040404', & ! '|'
      '18040402040418', & ! '}'
      '00000915120000' ] ! '~'

   !> Decoded glyph bits, lazily filled by ui_font_init().
   integer(int32), save :: glyph(FONT_H, FONT_COUNT)
   logical, save :: initialised = .false.

contains

   !> Decode the hex table once. Idempotent, cheap to call every frame.
   subroutine ui_font_init()
      integer :: i, row

      if (initialised) return
      do i = 1, FONT_COUNT
         do row = 1, FONT_H
            glyph(row, i) = int(hex_digit(FONT_HEX(i)(2*row - 1:2*row - 1))*16 &
                                + hex_digit(FONT_HEX(i)(2*row:2*row)), int32)
         end do
      end do
      initialised = .true.
   end subroutine ui_font_init

   !> Map an ASCII code to a 1..FONT_COUNT glyph index ('?' for anything else).
   pure integer function ui_font_char_index(code) result(idx)
      integer, intent(in) :: code

      idx = code - 31
      if (idx < 1 .or. idx > FONT_COUNT) idx = 32 - 31
   end function ui_font_char_index

   !> Return the seven row bitmasks of a glyph.
   function ui_font_rows(code) result(rows)
      integer, intent(in) :: code
      integer(int32) :: rows(FONT_H)

      if (.not. initialised) call ui_font_init()
      rows = glyph(:, ui_font_char_index(code))
   end function ui_font_rows

   !> Hex digit -> 0..15 value.
   pure integer function hex_digit(c) result(v)
      character(len=1), intent(in) :: c
      integer :: k

      k = index('0123456789abcdef', c)
      if (k == 0) k = index('0123456789ABCDEF', c)
      v = max(k - 1, 0)
   end function hex_digit

end module ui_font
