!> Abstract GPU renderer interface.
!>
!> The UI core produces one ui_batch (triangles + atlas). A backend only has to
!> take that batch and put it on screen. Adding a new GPU API means adding one
!> file in src/gfx - no other module has to change.
module ui_gfx
   use ui_kinds
   use ui_geom
   implicit none
   private

   public :: ui_gfx_backend

   !> Base class of every renderer backend.
   !>  begin(w,h)   - (re)create or resize the swap chain / render target
   !>  draw(batch)  - consume the frame geometry
   !>  present()    - swap buffers / end the pass
   !>  shutdown()   - release everything
   type, abstract :: ui_gfx_backend
      integer :: width = 0
      integer :: height = 0
   contains
      procedure(iface_begin), deferred :: begin
      procedure(iface_draw), deferred :: draw
      procedure(iface_present), deferred :: present
      procedure(iface_shutdown), deferred :: shutdown
      procedure(iface_name), deferred :: name
   end type ui_gfx_backend

   abstract interface

      subroutine iface_begin(self, width, height)
         import :: ui_gfx_backend
         class(ui_gfx_backend), intent(inout) :: self
         integer, intent(in) :: width, height
      end subroutine iface_begin

      subroutine iface_draw(self, batch)
         import :: ui_gfx_backend, ui_batch
         class(ui_gfx_backend), intent(inout) :: self
         type(ui_batch), intent(in), target :: batch
      end subroutine iface_draw

      subroutine iface_present(self)
         import :: ui_gfx_backend
         class(ui_gfx_backend), intent(inout) :: self
      end subroutine iface_present

      subroutine iface_shutdown(self)
         import :: ui_gfx_backend
         class(ui_gfx_backend), intent(inout) :: self
      end subroutine iface_shutdown

      function iface_name(self) result(s)
         import :: ui_gfx_backend
         class(ui_gfx_backend), intent(in) :: self
         character(len=:), allocatable :: s
      end function iface_name

   end interface

end module ui_gfx
