!> RGB8 image buffer and dependency free writers.
!> Used by the software backend and by headless tests. No C, no platform API.
module ui_img
   use ui_kinds
   implicit none
   private

   public :: ui_image, ui_image_init, ui_image_free, ui_image_get
   public :: ui_save_ppm, ui_save_bmp

   type :: ui_image
      integer :: w = 0
      integer :: h = 0
      integer(ip), allocatable :: px(:, :, :)
   end type ui_image

contains

   subroutine ui_image_init(img, w, h)
      type(ui_image), intent(inout) :: img
      integer, intent(in) :: w, h

      if (allocated(img%px)) deallocate (img%px)
      allocate (img%px(w, h, 3))
      img%px = 0
      img%w = w
      img%h = h
   end subroutine ui_image_init

   subroutine ui_image_free(img)
      type(ui_image), intent(inout) :: img

      if (allocated(img%px)) deallocate (img%px)
      img%w = 0
      img%h = 0
   end subroutine ui_image_free

   !> Read one pixel as a colour; out of range coordinates return black.
   pure function ui_image_get(img, x, y) result(c)
      type(ui_image), intent(in) :: img
      integer, intent(in) :: x, y
      type(ui_color) :: c

      if (x < 1 .or. x > img%w .or. y < 1 .or. y > img%h) then
         c = ui_color(0_ip, 0_ip, 0_ip, 255_ip)
      else
         c = ui_color(img%px(x, y, 1), img%px(x, y, 2), img%px(x, y, 3), 255_ip)
      end if
   end function ui_image_get

   !------------------------------------------------------------------
   ! writers
   !------------------------------------------------------------------

   !> Binary PPM (P6), the simplest spec-only raster format.
   subroutine ui_save_ppm(img, path, ierr)
      type(ui_image), intent(in) :: img
      character(*), intent(in) :: path
      integer, intent(out), optional :: ierr
      character(len=:), allocatable :: row, header
      character(len=16) :: ws, hs
      integer :: u, x, y, ios

      if (present(ierr)) ierr = 0
      if (.not. allocated(img%px)) then
         if (present(ierr)) ierr = 1
         return
      end if

      write (ws, '(i0)') img%w
      write (hs, '(i0)') img%h
      header = 'P6'//achar(10)//trim(ws)//' '//trim(hs)//achar(10)//'255'//achar(10)

      allocate (character(len=3*img%w) :: row)
      open (newunit=u, file=path, access='stream', form='unformatted', &
            status='replace', action='write', iostat=ios)
      if (ios /= 0) then
         if (present(ierr)) ierr = ios
         return
      end if

      write (u) header
      do y = 1, img%h
         do x = 1, img%w
            row(3*x - 2:3*x) = achar(iand(img%px(x, y, 1), 255_ip))// &
                               achar(iand(img%px(x, y, 2), 255_ip))// &
                               achar(iand(img%px(x, y, 3), 255_ip))
         end do
         write (u) row
      end do
      close (u)
   end subroutine ui_save_ppm

   !> Uncompressed 24bpp BMP, openable by the stock viewer of every OS.
   subroutine ui_save_bmp(img, path, ierr)
      type(ui_image), intent(in) :: img
      character(*), intent(in) :: path
      integer, intent(out), optional :: ierr
      character(len=:), allocatable :: buf
      integer :: u, x, y, ios, rowsize, offset, p, filesize

      if (present(ierr)) ierr = 0
      if (.not. allocated(img%px)) then
         if (present(ierr)) ierr = 1
         return
      end if

      rowsize = 4*((3*img%w + 3)/4)
      offset = 54
      filesize = offset + rowsize*img%h
      allocate (character(len=filesize) :: buf)
      buf = repeat(achar(0), filesize)

      buf(1:2) = 'BM'
      call put_le32(buf, 3, filesize)
      call put_le32(buf, 11, offset)
      call put_le32(buf, 15, 40)              ! DIB header size
      call put_le32(buf, 19, img%w)
      call put_le32(buf, 23, img%h)
      call put_le16(buf, 27, 1)               ! planes
      call put_le16(buf, 29, 24)              ! bits per pixel
      call put_le32(buf, 31, 0)               ! BI_RGB, no compression
      call put_le32(buf, 35, rowsize*img%h)
      call put_le32(buf, 39, 2835)            ! 72 dpi
      call put_le32(buf, 43, 2835)

      do y = 1, img%h
         p = offset + (img%h - y)*rowsize
         do x = 1, img%w
            buf(p + 1:p + 1) = achar(iand(img%px(x, y, 3), 255_ip))
            buf(p + 2:p + 2) = achar(iand(img%px(x, y, 2), 255_ip))
            buf(p + 3:p + 3) = achar(iand(img%px(x, y, 1), 255_ip))
            p = p + 3
         end do
      end do

      open (newunit=u, file=path, access='stream', form='unformatted', &
            status='replace', action='write', iostat=ios)
      if (ios /= 0) then
         if (present(ierr)) ierr = ios
         return
      end if
      write (u) buf
      close (u)
   end subroutine ui_save_bmp

   subroutine put_le32(buf, pos, v)
      character(len=*), intent(inout) :: buf
      integer, intent(in) :: pos, v
      integer :: i

      do i = 0, 3
         buf(pos + i:pos + i) = achar(iand(ishft(v, -8*i), 255))
      end do
   end subroutine put_le32

   subroutine put_le16(buf, pos, v)
      character(len=*), intent(inout) :: buf
      integer, intent(in) :: pos, v

      buf(pos:pos) = achar(iand(v, 255))
      buf(pos + 1:pos + 1) = achar(iand(ishft(v, -8), 255))
   end subroutine put_le16

end module ui_img
