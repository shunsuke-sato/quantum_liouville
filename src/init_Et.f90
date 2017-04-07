!---------------------------------------------------!
! Copyright (c) 2017 Shunsuke A. Sato               !
! Released under the MIT license                    !
! https://opensource.org/licenses/mit-license.php   !
!---------------------------------------------------!
subroutine init_Et
  use global_variables
  implicit none
  real(8) :: tt,xx
  integer :: it


  Et = 0d0
  do it = 0,Nt
    tt = dt*it
    if(tt <= Tpulse)then
      xx = tt - 0.5d0*Tpulse
      Et(3,it) = E0*sin(omega0*xx)*cos(pi*xx/Tpulse)**2
    end if
  end do

end subroutine init_Et
