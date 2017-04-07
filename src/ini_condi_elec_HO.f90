!---------------------------------------------------!
! Copyright (c) 2017 Shunsuke A. Sato               !
! Released under the MIT license                    !
! https://opensource.org/licenses/mit-license.php   !
!---------------------------------------------------!
subroutine ini_condi_elec_HO
  use global_variables
  implicit none
  real(8) :: xx,pp
  integer :: i

  do i = 1,3
    call gaussian_random_number(xx,pp)
    x_n(i) = xx/sqrt(2d0*alpha_HO); p_n(i) = pp/sqrt(2d0/alpha_HO)
  end do

end subroutine ini_condi_elec_HO
