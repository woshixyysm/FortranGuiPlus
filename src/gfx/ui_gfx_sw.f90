!> Software (CPU) renderer backend.
!>
!> Two rasterisers live here:
!>   * render_batch     - consumes the same ui_batch the GPU backends consume;
!>                        this is the reference implementation used for
!>                        headless validation of the geometry batcher.
!>   * render_drawlist  - draws the draw list directly (vector quality, used
!>                        for the offline image dumps).
!> Both use the continuous pixel convention of a GPU viewport (pixel i covers
!> [i-1, i)), so their output matches the hardware path.
module ui_gfx_sw
   use ui_kinds
   use ui_draw
   use ui_font
   use ui_geom
   use ui_img
   use ui_gfx
   implicit none
   private

   public :: ui_sw_backend

   type, extends(ui_gfx_backend) :: ui_sw_backend
      type(ui_image) :: img
      type(ui_color) :: clear = ui_color(0_ip, 0_ip, 0_ip, 255_ip)
   contains
      procedure :: begin => sw_begin
      procedure :: draw => sw_draw
      procedure :: present => sw_present
      procedure :: shutdown => sw_shutdown
      procedure :: name => sw_name
      procedure :: render_drawlist => sw_render_drawlist
      procedure :: render_batch => sw_render_batch
   end type ui_sw_backend

contains

   !------------------------------------------------------------------
   ! backend interface
   !------------------------------------------------------------------

   subroutine sw_begin(self, width, height)
      class(ui_sw_backend), intent(inout) :: self
      integer, intent(in) :: width, height

      if (self%width /= width .or. self%height /= height .or. &
          .not. allocated(self%img%px)) then
         call ui_image_init(self%img, width, height)
      end if
      self%width = width
      self%height = height
   end subroutine sw_begin

   subroutine sw_draw(self, batch)
      class(ui_sw_backend), intent(inout) :: self
      type(ui_batch), intent(in), target :: batch

      call sw_render_batch(self, batch)
   end subroutine sw_draw

   subroutine sw_present(self)
      class(ui_sw_backend), intent(inout) :: self

      ! nothing to swap: the image is the frame buffer
      if (.false.) print *, self%width
   end subroutine sw_present

   subroutine sw_shutdown(self)
      class(ui_sw_backend), intent(inout) :: self

      call ui_image_free(self%img)
      self%width = 0
      self%height = 0
   end subroutine sw_shutdown

   function sw_name(self) result(s)
      class(ui_sw_backend), intent(in) :: self
      character(len=:), allocatable :: s

      s = 'software'
      if (.false.) print *, self%width
   end function sw_name

   !------------------------------------------------------------------
   ! primitives
   !------------------------------------------------------------------

   subroutine put_px(img, x, y, c)
      type(ui_image), intent(inout) :: img
      integer, intent(in) :: x, y
      type(ui_color), intent(in) :: c
      real(wp) :: a

      if (x < 1 .or. x > img%w .or. y < 1 .or. y > img%h) return
      if (c%a <= 0) return
      if (c%a >= 255) then
         img%px(x, y, 1) = iand(c%r, 255_ip)
         img%px(x, y, 2) = iand(c%g, 255_ip)
         img%px(x, y, 3) = iand(c%b, 255_ip)
         return
      end if

      a = real(c%a, wp)/255.0_wp
      img%px(x, y, 1) = nint(real(c%r, wp)*a + real(img%px(x, y, 1), wp)*(1.0_wp - a))
      img%px(x, y, 2) = nint(real(c%g, wp)*a + real(img%px(x, y, 2), wp)*(1.0_wp - a))
      img%px(x, y, 3) = nint(real(c%b, wp)*a + real(img%px(x, y, 3), wp)*(1.0_wp - a))
   end subroutine put_px

   !> Pixel indices covered by the continuous span [x, x+w).
   pure integer function span_lo(x) result(i)
      real(wp), intent(in) :: x
      i = ceiling(x + 0.5_wp)
   end function span_lo

   pure integer function span_hi(x, w) result(i)
      real(wp), intent(in) :: x, w
      i = ceiling(x + w + 0.5_wp) - 1
   end function span_hi

   subroutine raster_rect(img, x, y, w, h, c)
      type(ui_image), intent(inout) :: img
      real(wp), intent(in) :: x, y, w, h
      type(ui_color), intent(in) :: c
      integer :: i, j, i0, i1, j0, j1

      if (w <= 0.0_wp .or. h <= 0.0_wp) return
      i0 = span_lo(x)
      i1 = span_hi(x, w)
      j0 = span_lo(y)
      j1 = span_hi(y, h)
      if (i1 < i0) i1 = i0
      if (j1 < j0) j1 = j0
      do j = max(j0, 1), min(j1, img%h)
         do i = max(i0, 1), min(i1, img%w)
            call put_px(img, i, j, c)
         end do
      end do
   end subroutine raster_rect

   subroutine raster_rect_outline(img, x, y, w, h, c, thickness)
      type(ui_image), intent(inout) :: img
      real(wp), intent(in) :: x, y, w, h
      type(ui_color), intent(in) :: c
      real(wp), intent(in) :: thickness
      real(wp) :: t

      t = max(thickness, 1.0_wp)
      call raster_rect(img, x, y, w, t, c)
      call raster_rect(img, x, y + h - t, w, t, c)
      call raster_rect(img, x, y, t, h, c)
      call raster_rect(img, x + w - t, y, t, h, c)
   end subroutine raster_rect_outline

   subroutine raster_line(img, x0, y0, x1, y1, c, thickness)
      type(ui_image), intent(inout) :: img
      real(wp), intent(in) :: x0, y0, x1, y1
      type(ui_color), intent(in) :: c
      real(wp), intent(in) :: thickness
      integer :: ix0, iy0, ix1, iy1, dx, dy, sx, sy, err, e2, guard, t

      t = max(nint(thickness), 1)
      ix0 = nint(x0); iy0 = nint(y0)
      ix1 = nint(x1); iy1 = nint(y1)
      dx = abs(ix1 - ix0)
      dy = abs(iy1 - iy0)
      sx = 1
      if (ix1 < ix0) sx = -1
      sy = 1
      if (iy1 < iy0) sy = -1
      err = dx - dy

      guard = 0
      do
         call raster_rect(img, real(ix0, wp) - real(t, wp)*0.5_wp, &
                          real(iy0, wp) - real(t, wp)*0.5_wp, &
                          real(t, wp), real(t, wp), c)
         if (ix0 == ix1 .and. iy0 == iy1) exit
         guard = guard + 1
         if (guard > 100000) exit
         e2 = 2*err
         if (e2 > -dy) then
            err = err - dy
            ix0 = ix0 + sx
         end if
         if (e2 < dx) then
            err = err + dx
            iy0 = iy0 + sy
         end if
      end do
   end subroutine raster_line

   subroutine raster_circle(img, cx, cy, radius, c, filled, thickness)
      type(ui_image), intent(inout) :: img
      real(wp), intent(in) :: cx, cy, radius
      type(ui_color), intent(in) :: c
      logical, intent(in) :: filled
      real(wp), intent(in) :: thickness
      integer :: i, j, i0, i1, j0, j1
      real(wp) :: d, r2, ri2

      r2 = radius*radius
      ri2 = max(radius - max(thickness, 1.0_wp), 0.0_wp)**2
      i0 = floor(cx - radius); i1 = ceiling(cx + radius)
      j0 = floor(cy - radius); j1 = ceiling(cy + radius)

      do j = max(j0, 1), min(j1, img%h)
         do i = max(i0, 1), min(i1, img%w)
            d = (real(i, wp) - 0.5_wp - cx)**2 + (real(j, wp) - 0.5_wp - cy)**2
            if (filled) then
               if (d <= r2) call put_px(img, i, j, c)
            else
               if (d <= r2 .and. d >= ri2) call put_px(img, i, j, c)
            end if
         end do
      end do
   end subroutine raster_circle

   subroutine raster_text(img, x, y, s, c, scale)
      type(ui_image), intent(inout) :: img
      real(wp), intent(in) :: x, y
      character(*), intent(in) :: s
      type(ui_color), intent(in) :: c
      real(wp), intent(in) :: scale
      integer :: i, row, col, sc
      integer(ip) :: rows(FONT_H)

      sc = max(nint(scale), 1)

      do i = 1, len_trim(s)
         rows = ui_font_rows(iachar(s(i:i)))
         do row = 1, FONT_H
            do col = 0, FONT_W - 1
               if (btest(rows(row), FONT_W - 1 - col)) then
                  call raster_rect(img, &
                     x + real((i - 1)*UI_FONT_ADVANCE*sc + col*sc, wp), &
                     y + real((row - 1)*sc, wp), &
                     real(sc, wp), real(sc, wp), c)
               end if
            end do
         end do
      end do
   end subroutine raster_text

   !------------------------------------------------------------------
   ! frame rendering
   !------------------------------------------------------------------

   !> Vector path: play the draw list straight into the image.
   subroutine sw_render_drawlist(self, dl)
      class(ui_sw_backend), intent(inout) :: self
      type(ui_drawlist), intent(in) :: dl
      integer :: i

      if (.not. allocated(self%img%px)) return
      call ui_font_init()
      call clear_image(self)

      do i = 1, dl%count
         select case (dl%cmd(i)%kind)
         case (UI_DRAW_RECT)
            call raster_rect(self%img, dl%cmd(i)%a(1), dl%cmd(i)%a(2), &
                             dl%cmd(i)%b(1), dl%cmd(i)%b(2), dl%cmd(i)%fg)
         case (UI_DRAW_RECT_OUTLINE)
            call raster_rect_outline(self%img, dl%cmd(i)%a(1), dl%cmd(i)%a(2), &
                                     dl%cmd(i)%b(1), dl%cmd(i)%b(2), dl%cmd(i)%fg, &
                                     dl%cmd(i)%thickness)
         case (UI_DRAW_TEXT)
            call raster_text(self%img, dl%cmd(i)%a(1), dl%cmd(i)%a(2), &
                             dl%cmd(i)%text, dl%cmd(i)%fg, dl%cmd(i)%scale)
         case (UI_DRAW_LINE)
            call raster_line(self%img, dl%cmd(i)%a(1), dl%cmd(i)%a(2), &
                             dl%cmd(i)%b(1), dl%cmd(i)%b(2), dl%cmd(i)%fg, &
                             dl%cmd(i)%thickness)
         case (UI_DRAW_CIRCLE)
            call raster_circle(self%img, dl%cmd(i)%a(1), dl%cmd(i)%a(2), &
                               dl%cmd(i)%b(1), dl%cmd(i)%fg, .true., 1.0_wp)
         case (UI_DRAW_CIRCLE_OUTLINE)
            call raster_circle(self%img, dl%cmd(i)%a(1), dl%cmd(i)%a(2), &
                               dl%cmd(i)%b(1), dl%cmd(i)%fg, .false., &
                               dl%cmd(i)%thickness)
         end select
      end do
   end subroutine sw_render_drawlist

   !> Triangle path: exactly what the GPU backends draw.
   subroutine sw_render_batch(self, batch)
      class(ui_sw_backend), intent(inout) :: self
      type(ui_batch), intent(in), target :: batch
      integer(ip), allocatable :: tex(:, :)
      real(wp) :: a(2), b(2), c(2), pa(2, 3), uv(2, 3), ca(4, 3)
      real(wp) :: area, w0, w1, w2, px, py, u, v, cr, cg, cb, al, cov
      integer :: k, i, j, i0, i1, j0, j1, tx, ty

      if (.not. allocated(self%img%px)) return
      if (batch%count < 3) return

      call clear_image(self)
      allocate (tex(UI_ATLAS_W, UI_ATLAS_H))
      call ui_atlas_build(tex)

      k = 1
      do while (k + 2 <= batch%count)
         do i = 1, 3
            pa(1, i) = (real(batch%v(k + i - 1)%x, wp) + 1.0_wp)*0.5_wp*self%img%w
            pa(2, i) = (1.0_wp - real(batch%v(k + i - 1)%y, wp))*0.5_wp*self%img%h
            uv(1, i) = real(batch%v(k + i - 1)%u, wp)
            uv(2, i) = real(batch%v(k + i - 1)%v, wp)
            ca(1, i) = real(batch%v(k + i - 1)%r, wp)
            ca(2, i) = real(batch%v(k + i - 1)%g, wp)
            ca(3, i) = real(batch%v(k + i - 1)%b, wp)
            ca(4, i) = real(batch%v(k + i - 1)%a, wp)
         end do
         a = pa(:, 1)
         b = pa(:, 2)
         c = pa(:, 3)
         area = (b(1) - a(1))*(c(2) - a(2)) - (b(2) - a(2))*(c(1) - a(1))

         if (abs(area) > 1.0e-9_wp) then
            i0 = max(1, floor(min(a(1), b(1), c(1))))
            i1 = min(self%img%w, ceiling(max(a(1), b(1), c(1))))
            j0 = max(1, floor(min(a(2), b(2), c(2))))
            j1 = min(self%img%h, ceiling(max(a(2), b(2), c(2))))
            do j = j0, j1
               py = real(j, wp) - 0.5_wp
               do i = i0, i1
                  px = real(i, wp) - 0.5_wp
                  w0 = ((b(1) - px)*(c(2) - py) - (b(2) - py)*(c(1) - px))/area
                  w1 = ((c(1) - px)*(a(2) - py) - (c(2) - py)*(a(1) - px))/area
                  w2 = 1.0_wp - w0 - w1
                  if (w0 < 0.0_wp .or. w1 < 0.0_wp .or. w2 < 0.0_wp) cycle

                  u = w0*uv(1, 1) + w1*uv(1, 2) + w2*uv(1, 3)
                  v = w0*uv(2, 1) + w1*uv(2, 2) + w2*uv(2, 3)
                  tx = min(max(int(u*real(UI_ATLAS_W, wp)) + 1, 1), UI_ATLAS_W)
                  ty = min(max(int(v*real(UI_ATLAS_H, wp)) + 1, 1), UI_ATLAS_H)
                  cov = real(tex(tx, ty), wp)/255.0_wp
                  if (cov <= 0.0_wp) cycle

                  al = (w0*ca(4, 1) + w1*ca(4, 2) + w2*ca(4, 3))*cov
                  if (al <= 0.0_wp) cycle
                  cr = w0*ca(1, 1) + w1*ca(1, 2) + w2*ca(1, 3)
                  cg = w0*ca(2, 1) + w1*ca(2, 2) + w2*ca(2, 3)
                  cb = w0*ca(3, 1) + w1*ca(3, 2) + w2*ca(3, 3)

                  self%img%px(i, j, 1) = nint(cr*255.0_wp*al + &
                     real(self%img%px(i, j, 1), wp)*(1.0_wp - al))
                  self%img%px(i, j, 2) = nint(cg*255.0_wp*al + &
                     real(self%img%px(i, j, 2), wp)*(1.0_wp - al))
                  self%img%px(i, j, 3) = nint(cb*255.0_wp*al + &
                     real(self%img%px(i, j, 3), wp)*(1.0_wp - al))
               end do
            end do
         end if
         k = k + 3
      end do

      deallocate (tex)
   end subroutine sw_render_batch

   subroutine clear_image(self)
      class(ui_sw_backend), intent(inout) :: self

      self%img%px(:, :, 1) = iand(self%clear%r, 255_ip)
      self%img%px(:, :, 2) = iand(self%clear%g, 255_ip)
      self%img%px(:, :, 3) = iand(self%clear%b, 255_ip)
   end subroutine clear_image

end module ui_gfx_sw
