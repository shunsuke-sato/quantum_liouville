!---------------------------------------------------!
! Copyright (c) 2017 Shunsuke A. Sato               !
! Released under the MIT license                    !
! https://opensource.org/licenses/mit-license.php   !
!---------------------------------------------------!
program main
  use global_variables
  implicit none

  call initialize_mpi
  call initialize_parameters

  call QL_dynamics_HO

  call MPI_finalize(ierr)

end program main
