program main
  implicit none
  real(8),parameter :: pi=4d0*atan(1d0)
  complex(8),parameter :: zI=(0d0,1d0)
  integer,parameter :: Nt = 50000, Nw = 6000
!  real(8),parameter :: ww_max = 120d0/27.211d0, dw = ww_max/Nw
  real(8),parameter :: ww_max = 300d0, dw = ww_max/Nw
  real(8)  :: Tprop = 100d0/0.02418d0
  real(8) :: tt(0:Nt),pz(0:Nt),weight(0:Nt)
  integer :: it,iw
  real(8) :: f1,f2,f3,f4,f5,f6,f7,f8,xx,ww,dt
  complex(8) :: zpzw


  do it = 0,Nt
    read(*,*)tt(it),f1,f2,f3,f4,f5,f6,f7,f8,pz(it)
  end do
  dt = tt(1) - tt(0)
  Tprop = tt(Nt)

  weight = 0d0
  do it = 0,Nt
!    if(tt(it) < 0.5d0*Tprop)then
!      weight(it) = 1d0
!    else 
!      xx = (tt(it)-0.5d0*Tprop)/(tt(Nt)-0.5d0*Tprop)
      xx = (tt(it)-tt(0))/(tt(Nt)-tt(0))
!      weight(it) = 1d0 - 3d0*xx**2 + 2d0*xx**3
!      weight(it) = exp(-1d0/27.2d0*(tt(it)-Tprop))
!    end if
  end do
  weight = 1d0

  open(21,file="zpzw.out")
  do iw = 0,Nw
    ww = dw*iw
    zpzw = 0d0
    do it = 0,Nt
      zpzw = zpzw +exp(zI*ww*tt(it))*weight(it)*pz(it)
    end do
    zpzw = zpzw*dt
    write(21,"(999e26.16e3)")ww,abs(zpzw)**2
  end do
  close(21)


end program main
