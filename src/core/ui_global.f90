!> The "static lazy pointer" for the UI context.
!>
!> Fortran has no `static` keyword, but a module variable has exactly the
!> lifetime of a C static: it is created before the program starts and lives
!> until it ends. Combining SAVE + TARGET + a POINTER that is wired up on
!> first use gives the C idiom
!>
!>     static ui_ctx* g = NULL;
!>     ui_ctx* ctx(void) { if (!g) g = &default_ctx; return g; }
!>
!> The point is convenience: a deep call tree does not have to thread the
!> context through every argument. Widgets stay pure functions of the context,
!> the context just becomes reachable from anywhere.
!>
!>   type(ui_ctx), pointer :: ui
!>   ui => ui_ctx_current()      ! first call lazily binds the singleton
!>   call ui%begin_frame(input)
!>
!> or, with an explicit context of your own:
!>
!>   call ui_ctx_bind(my_ctx)    ! ui_ctx_current() now returns my_ctx
module ui_global
   use ui_kinds
   use ui
   implicit none
   private

   public :: ui_ctx_current, ui_ctx_bind, ui_ctx_release, ui_ctx_is_lazy

   !> Lazy singleton storage. Created by the compiler, never by the user.
   type(ui_ctx), save, target :: g_default_ctx

   !> The lazy pointer itself. NULL until the first ui_ctx_current() call.
   type(ui_ctx), pointer, save :: g_ctx => null()

   !> True while the pointer refers to the built-in singleton.
   logical, save :: g_is_default = .false.

contains

   !> Return the current context, binding the singleton on first use.
   !> This is the whole trick: one SAVE'd pointer, checked lazily.
   function ui_ctx_current() result(p)
      type(ui_ctx), pointer :: p

      if (.not. associated(g_ctx)) then
         g_ctx => g_default_ctx
         g_is_default = .true.
      end if
      p => g_ctx
   end function ui_ctx_current

   !> Bind an application owned context instead (no copy is made).
   subroutine ui_ctx_bind(ctx)
      type(ui_ctx), target, intent(inout) :: ctx

      g_ctx => ctx
      g_is_default = .false.
   end subroutine ui_ctx_bind

   !> Forget the current binding; the next ui_ctx_current() rebinds the
   !> built-in singleton.
   subroutine ui_ctx_release()
      g_ctx => null()
      g_is_default = .false.
   end subroutine ui_ctx_release

   !> True when the pointer currently refers to the lazily created singleton.
   logical function ui_ctx_is_lazy() result(r)

      r = g_is_default
   end function ui_ctx_is_lazy

end module ui_global
