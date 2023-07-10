!--------------------------------------------------------------------------!
! Simple SPH by Daniel Elsender, 2023.                                     !  
!--------------------------------------------------------------------------!

module denisty
! this module is the guts of the code
! It calculates the density by iteration of smoothing length
!
!
    
   implicit none

   contains

   subroutine get_density(xyzh,i,rho,w,npart)

      use kernel, only : get_kernel

   ! define variables required to calculate density
      integer :: i,j,k

      real :: dx,dy,dz,rij2,rij,hij,q,q2
      real :: wkern,grkern

      integer, intent(in) :: npart

      real, intent(out) :: rho(:)
      real, intent(inout) :: w(:,:)

   ! get particle neighbours 
      ! call get_neighbours()
   ! for now just use all particles
      nneigh = npart

   ! calculate density

   ! initialise density
      rho(:) = 0.0
      do j = 1, nneigh ! how do we get nneigh?
   ! get the kernel value
         dx = xyzh(1,i) - xyzh(1,j)
         dy = xyzh(2,i) - xyzh(2,j)
         dz = xyzh(3,i) - xyzh(3,j)
         rij2 = dx * dx + dy * dy + dz * dz
         rij = sqrt(rij2)
         hij 
         q = rij * hij
         q2 = q * q
         call get_kernel(q2,q,wkern,grkern)
         k = neigh(i,j)
         rho(i) = rho(i) + mass(k) * w(i,j)
      end do
   
   end subroutine get_density

    



    
end module denisty