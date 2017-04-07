!---------------------------------------------------!
! Copyright (c) 2017 Shunsuke A. Sato               !
! Released under the MIT license                    !
! https://opensource.org/licenses/mit-license.php   !
!---------------------------------------------------!
subroutine initialize_parameters
  use global_variables
  implicit none

! Time propagation
  Nt = 50000
  Tprop = 120d0/0.02418d0
  dt = Tprop/Nt

! Number of trajectory
  Ntraj = 100000

! Parameters of model
  Ip_HO = 1d0 !15.6d0/27.211d0
  alpha_HO = Ip_HO/10d0 !2d0*Ip_HO
  beta_HO = Ip_HO + 3d0*alpha_HO/2d0

  omega0 = alpha_HO/10.5d0 !1.d0/27.211d0
  Tpulse = 100d0/0.02418d0
  Up = Ip_HO !20d0/27.211d0
  E0 = sqrt(4d0*Up*omega0**2)


  allocate(x_l(3,0:Nt),p_l(3,0:Nt),x_ave(3,0:Nt),p_ave(3,0:Nt))
  allocate(Et(3,0:Nt))

  call init_Et

end subroutine initialize_parameters
