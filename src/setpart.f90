!-----------------------------------!
! A simple 3D SPH code to solve the !
! Navier-Stokes equations           !
!                                   !
! Author: Daniel Elsender           ! 
! Date: 26/06/2023                  !
!                                   !
!-----------------------------------!

module setpart
	implicit none

   public :: unifdis

   contains

   subroutine unifdis(xyzh,xmin,xmax,ymin,ymax,zmin,zmax,delta,hfact)

      integer :: ipart, i, j, k, nx, ny, nz
      real :: xi, yi, zi
      real :: deltax, deltay, deltaz
      real :: dxbound, dybound, dzbound

      integer, intent(out) :: npart

      real, intent(in) :: xmin, xmax, ymin, ymax, zmin, zmax, delta, hfact

      real, intent(out), dimension(:,:) :: xyzh
      real, intent(out), dimension(:,:) :: vxyz

   ! define variables needed


   ! setup the domain

      dxbound = xmax - xmin
      dybound = ymax - ymin
      dzbound = zmax - zmin

      nx = int(dxbound/delta)
      ny = int(dybound/delta)
      nz = int(dzbound/delta)

      deltax = dxbound/nx
      deltay = dybound/ny
      deltaz = dzbound/nz

   ! setup the particles

      ipart = 0

      do i = 1, nz
         zi = zmin + (i - 0.5)*deltaz
         do j = 1, ny
            yi = ymin + (j - 0.5)*deltay
            do k = 1, nx
               ipart = ipart + 1
               xi = xmin + (k - 0.5)*deltax
               xyzh(1,ipart) = xi
               xyzh(2,ipart) = yi
               xyzh(3,ipart) = zi
               xyzh(4,ipart) = hfact * deltax
            end do
         end do
      end do
      npart = ipart
   ! setup the boundary particles


   ! give the particles their initial values
      call colliding_streams(xyzh,vxyz,npart)

   end subroutine unifdis
end module setpart