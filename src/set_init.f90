!-----------------------------------!
! A simple 3D SPH code to solve the !
! Navier-Stokes equations           !
!                                   !
! Author: Daniel Elsender           ! 
! Date: 26/06/2023                  !
!                                   !
!-----------------------------------!

module set_init
   implicit none

   public :: colliding_streams

   contains
      subroutine colliding_streams(xyzh,vxyz,npart)

         integer :: i
         integer, intent(in) :: npart
         real, intent(in) :: xyzh(:,:)
         real, intent(out) :: vxyz(:,:)
! give initial coniditions for colliding streams of gas
         
! loop over all particles and set initial conditions
         do i=1,npart
            if (xyzh(1,i) < 0.0) then
               vxyz(1,i) = 1.0
            else
               vxyz(1,i) = -1.0
            end if
            vxyz(2,i) = 0.0
            vxyz(3,i) = 0.0
         end do
      end subroutine colliding_streams

end module set_init