!
! analytical calculations of the 3 valley QD in anisotropic effective mass model
! cube infitine barrier
!


 program test_3_valley
 implicit none
 
 ! constants
 double precision, parameter :: h_p=1.05D-27  ! SGS system
 double precision, parameter :: m_0=0.91D-27  
 
 double precision, parameter :: eV=1.6D-12
 double precision, parameter :: nm=1.0D-7
 double precision, parameter :: pi=3.14159265358979323846264
 
 double precision, parameter :: E_unit_eV=h_p*h_p*pi*pi/2.0/m_0/nm/nm/eV
 
 
 double precision, parameter :: m_per=0.19D0
 double precision, parameter :: m_par=0.98D0
 
 double precision :: Lx, Ly, Lz
 
 integer :: ix, ix_max, iy, iy_max, iz, iz_max
 
 Lx=20.0
 Ly=30.0
 Lz=10.0
 
 ix_max=2
 iy_max=2
 iz_max=2
 
 do ix=1,ix_max
  do iy=1,iy_max
   do iz=1,iz_max
   
     write(*,'(3I3,3E15.7)')ix,iy,iz, E_unit_eV*(ix*ix/Lx/Lx/m_par + iy*iy/Ly/Ly/m_per + iz*iz/Lz/Lz/m_per  ), &
                                E_unit_eV*(ix*ix/Lx/Lx/m_per + iy*iy/Ly/Ly/m_par + iz*iz/Lz/Lz/m_per  ),       &
                                E_unit_eV*(ix*ix/Lx/Lx/m_per + iy*iy/Ly/Ly/m_per + iz*iz/Lz/Lz/m_par  ) 
   end do
  end do
 end do
 
 end program test_3_valley