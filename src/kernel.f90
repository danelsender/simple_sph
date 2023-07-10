!-----------------------------------!
! A simple 3D SPH code to solve the !
! Navier-Stokes equations           !
!                                   !
! Author: Daniel Elsender           ! 
! Date: 26/06/2023                  !
!                                   !
!-----------------------------------!

module kernel

   implicit none

   contains

   pure subroutine get_kernel(q2,q,wkern,grkern)

   real, intent(in) :: q2,q
   real, intent(out) :: wkern,grkern

   if (q < 1.) then
      wkern  = 0.75*q2*q - 1.5*q2 + 1.
      grkern = q*(2.25*q - 3.)
   elseif (q < 2.) then
      wkern  = -0.25*(q - 2.)**3
      grkern = -0.75*(q - 2.)**2
   else
      wkern  = 0.
      grkern = 0.
   endif

end module kernel