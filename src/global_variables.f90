!---------------------------------------------------!
! Copyright (c) 2017 Shunsuke A. Sato               !
! Released under the MIT license                    !
! https://opensource.org/licenses/mit-license.php   !
!---------------------------------------------------!
module global_variables
  implicit none
! Mathematical parameters
!  real(8),parameter :: pi=3.14159265358979323846d0
  real(8),parameter :: pi=4d0*atan(1d0)
  complex(8),parameter :: zI=(0d0,1d0)

! Control parameter
  character(8) :: calc_mode

! Parameters of model
  real(8) :: Ip_HO = 24.6d0/27.211d0
  real(8) :: alpha_HO, beta_HO

! Multi-trajectory
  integer :: Ntraj
  real(8) :: x_n(3),p_n(3)
  real(8),allocatable :: x_l(:,:),p_l(:,:),x_ave(:,:),p_ave(:,:)

! Time propagation
  integer :: Nt
  real(8) :: dt, Tprop

! Laser parameters
  real(8) :: Up,E0,omega0,Tpulse
  real(8),allocatable :: Et(:,:)

! MPI
  include 'mpif.h'
  integer :: Myrank,Nprocs,ierr


end module global_variables
