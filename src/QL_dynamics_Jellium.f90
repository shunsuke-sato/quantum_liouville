!---------------------------------------------------!
! Copyright (c) 2017 Shunsuke A. Sato               !
! Released under the MIT license                    !
! https://opensource.org/licenses/mit-license.php   !
!---------------------------------------------------!
subroutine QL_dynamics_Jellium
  use global_variables
  implicit none
  integer :: it,itraj
  integer :: npos_l,npos
  real(8) :: Etot_t, force_t(3),Etot_l,Etot0,r2,r1

  npos_l = 0
  Etot_l = 0d0
  x_l = 0d0; p_l = 0d0

  do itraj = 1,Ntraj
    if(myrank == 0)write(*,*)itraj,"/",Ntraj

    call ini_condi_elec_JL
    if(mod(itraj, Nprocs) /= myrank)cycle
    Etot_t = 0.5d0*sum(p_n**2)
    r2 = sum(x_n**2)
    if(r2 < R0_JL**2 )then
      Etot_t = Etot_t + 0.5d0*K_JL*r2 + C_JL
    else
      Etot_t = Etot_t - Q_JL/sqrt(r2)
    end if


    Etot_l = Etot_l + Etot_t

    x_l(:,0) = x_l(:,0) + x_n(:)
    p_l(:,0) = p_l(:,0) + p_n(:)

    do it = 0,Nt-1

      r2 = sum(x_n**2)
      if(r2 < R0_JL**2 )then
        force_t(:) = -K_JL*x_n(:) - Et(:,it)
      else
        r1 = sqrt(r2)
        force_t(:) = -Q_JL*x_n(:)/(r2*r1) - Et(:,it)
      end if
      p_n = p_n + 0.5d0*force_t*dt
      x_n = x_n + p_n*dt

      r2 = sum(x_n**2) 
      if(r2 < R0_JL**2 )then
        force_t(:) = -K_JL*x_n(:) - Et(:,it+1)
      else
        r1 = sqrt(r2)
        force_t(:) = -Q_JL*x_n(:)/(r2*r1) - Et(:,it+1)
      end if

      p_n = p_n + 0.5d0*force_t*dt
      
      x_l(:,it+1) = x_l(:,it+1) + x_n(:)
      p_l(:,it+1) = p_l(:,it+1) + p_n(:)

    end do

  end do

  call MPI_ALLREDUCE(npos_l,npos,1,MPI_INTEGER,MPI_SUM,MPI_COMM_WORLD,ierr)
  call MPI_ALLREDUCE(x_l,x_ave,3*(Nt+1),MPI_REAL8,MPI_SUM,MPI_COMM_WORLD,ierr)
  call MPI_ALLREDUCE(p_l,p_ave,3*(Nt+1),MPI_REAL8,MPI_SUM,MPI_COMM_WORLD,ierr)
  call MPI_ALLREDUCE(Etot_l,Etot0,1,MPI_REAL8,MPI_SUM,MPI_COMM_WORLD,ierr)
  x_ave = x_ave/Ntraj; p_ave = p_ave/Ntraj
  Etot0 = Etot0/Ntraj
  
  if(myrank == 0)then
    write(*,"(A,2x,I10,A,I10,e16.6e3,A)")"pos-orbit",npos,"/",Ntraj,100*npos/dble(Ntraj),"%"
    write(*,"(A,2x,e16.6e3)")"Etot0/Ip_HO",Etot0/Ip_HO
    open(31,file="xpt.out")
    do it = 0,Nt
      write(31,"(999e26.16e3)")dt*it,Et(:,it),x_ave(:,it),p_ave(:,it)
    end do
    close(31)
  end if

end subroutine QL_dynamics_Jellium
