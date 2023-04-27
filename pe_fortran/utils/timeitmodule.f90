module timeitmodule
  implicit none
  private
  public :: timeit
  interface timeit
     module procedure time0i2
     module procedure time0i4
     module procedure time0i8
     module procedure timei4i4
     module procedure timei8i4
     module procedure timei2i8
     module procedure timei2i2
  end interface timeit
contains

  integer(kind=8) function getnruns(elapsed)
    implicit none
    real(kind=8), parameter :: lim1ns=1.E-9, lim1us=1.E-6, lim1ms=1.E-3, lim20ms=2.E-2, &
         lim0p2s=0.2E-9, lim1s=1.
    real(kind=8) :: elapsed
    if (elapsed < lim1ns) then
       getnruns = 1000000
    else if (elapsed < lim1us) then
       getnruns = 10000
    else if (elapsed < lim1ms) then
       getnruns = 1000
    else if (elapsed < lim20ms) then
       getnruns = 200
    else if (elapsed < lim0p2s) then
       getnruns = 10
    else if (elapsed < lim1s) then
       getnruns = 5
    else
       getnruns = 1
    end if
  end function getnruns

  subroutine time0i8(f)
    implicit none
    interface
       function f() result (res)
         implicit none
         integer(kind=8) :: res
       end function f
    end interface
    integer(8) :: out
    integer(kind=8) :: start, finish, timeres, nruns, i
    real(kind=8) :: elapsed, mean
    logical :: test
    call system_clock(start, timeres)
    out = f()
    call system_clock(finish)
    elapsed = (dble(finish - start)/dble(timeres))
    nruns = getnruns(elapsed)
    timeloop : select case (nruns)
    case (1)
       mean = elapsed
    case default
       call system_clock(start, timeres)
       floop : do i = 1,nruns
          out = f()
       end do floop
       call system_clock(finish)
       mean = (dble(finish - start)/(dble(timeres)*dble(nruns)))
    end select timeloop
    write(*,"(A60)") "------------------------------------------------------------"
    write(*,"(A,I0,A,E9.3E2,A)") " Mean runtime (", nruns, " runs) = ", mean, " seconds"
    write(*,"(A)") " Result: "
    write(*,"(I30)") out
    write(*,"(A60)") "------------------------------------------------------------"
  end subroutine time0i8

  subroutine time0i4(f)
    implicit none
    interface
       function f() result (res)
         implicit none
         integer(kind=4) :: res
       end function f
    end interface
    integer(4) :: out
    integer(kind=8) :: start, finish, timeres
    integer(kind=8) :: nruns, i
    real(kind=8) :: elapsed, mean
    call system_clock(start, timeres)
    out = f()
    call system_clock(finish)
    elapsed = (dble(finish - start)/dble(timeres))
    nruns = getnruns(elapsed)
    timeloop : select case (nruns)
    case (1)
       mean = elapsed
    case default
       call system_clock(start, timeres)
       floop : do i = 1,nruns
          out = f()
       end do floop
       call system_clock(finish)
       mean = (dble(finish - start)/(dble(timeres)*dble(nruns)))
    end select timeloop
    write(*,"(A60)") "------------------------------------------------------------"
    write(*,"(A,I0,A,E9.3E2,A)") " Mean runtime (", nruns, " runs) = ", mean, " seconds"
    write(*,"(A)") " Result: "
    write(*,"(I30)") out
    write(*,"(A60)") "------------------------------------------------------------"
  end subroutine time0i4

  subroutine time0i2(f)
    implicit none
    interface
       function f() result (res)
         implicit none
         integer(kind=2) :: res
       end function f
    end interface
    integer(2) :: out
    integer(kind=8) :: start, finish, timeres
    integer(kind=8) :: nruns, i
    real(kind=8) :: elapsed, mean
    call system_clock(start, timeres)
    out = f()
    call system_clock(finish)
    elapsed = (dble(finish - start)/dble(timeres))
    nruns = getnruns(elapsed)
    timeloop : select case (nruns)
    case (1)
       mean = elapsed
    case default
       call system_clock(start, timeres)
       floop : do i = 1,nruns
          out = f()
       end do floop
       call system_clock(finish)
       mean = (dble(finish - start)/(dble(timeres)*dble(nruns)))
    end select timeloop
    write(*,"(A60)") "------------------------------------------------------------"
    write(*,"(A,I0,A,E9.3E2,A)") " Mean runtime (", nruns, " runs) = ", mean, " seconds"
    write(*,"(A)") " Result: "
    write(*,"(I30)") out
    write(*,"(A60)") "------------------------------------------------------------"
  end subroutine time0i2

  subroutine timei8i4(f, arg1)
    implicit none
    interface
       function f(arg1) result (res)
         implicit none
         integer(kind=8), intent(in) :: arg1
         integer(kind=4) :: res
       end function f
    end interface
    integer(4) :: out
    integer(8) :: arg1
    integer(kind=8) :: start, finish, timeres
    integer(kind=8) :: nruns, i
    real(kind=8) :: elapsed, mean
    call system_clock(start, timeres)
    out = f(arg1)
    call system_clock(finish)
    elapsed = (dble(finish - start)/dble(timeres))
    nruns = getnruns(elapsed)
    timeloop : select case (nruns)
    case (1)
       mean = elapsed
    case default
       call system_clock(start, timeres)
       floop : do i = 1,nruns
          out = f(arg1)
       end do floop
       call system_clock(finish)
       mean = (dble(finish - start)/(dble(timeres)*dble(nruns)))
    end select timeloop
    write(*,"(A60)") "------------------------------------------------------------"
    write(*,"(A,I0,A,E9.3E2,A)") " Mean runtime (", nruns, " runs) = ", mean, " seconds"
    write(*,"(A)") " Result: "
    write(*,"(I30)") out
    write(*,"(A60)") "------------------------------------------------------------"
  end subroutine timei8i4

  subroutine timei4i4(f, arg1)
    implicit none
    interface
       function f(arg1) result (out)
         implicit none
         integer(kind=4), intent(in) :: arg1
         integer(kind=4) :: out
       end function f
    end interface
    integer(4) :: arg1
    integer(4) :: out
    integer(kind=8) :: start, finish, timeres
    integer(kind=8) :: nruns, i
    real(kind=8) :: elapsed, mean
    call system_clock(start, timeres)
    out = f(arg1)
    call system_clock(finish)
    elapsed = (dble(finish - start)/dble(timeres))
    nruns = getnruns(elapsed)
    timeloop : select case (nruns)
    case (1)
       mean = elapsed
    case default
       call system_clock(start, timeres)
       floop : do i = 1,nruns
          out = f(arg1)
       end do floop
       call system_clock(finish)
       mean = (dble(finish - start)/(dble(timeres)*dble(nruns)))
    end select timeloop
    write(*,"(A60)") "------------------------------------------------------------"
    write(*,"(A,I0,A,E9.3E2,A)") " Mean runtime (", nruns, " runs) = ", mean, " seconds"
    write(*,"(A)") " Result: "
    write(*,"(I30)") out
    write(*,"(A60)") "------------------------------------------------------------"
  end subroutine timei4i4

  subroutine timei2i8(f, arg1)
    implicit none
    interface
       function f(arg1) result (out)
         implicit none
         integer(kind=2), intent(in) :: arg1
         integer(kind=8) :: out
       end function f
    end interface
    integer(2) :: arg1
    integer(8) :: out
    integer(kind=8) :: start, finish, timeres
    integer(kind=8) :: nruns, i
    real(kind=8) :: elapsed, mean
    call system_clock(start, timeres)
    out = f(arg1)
    call system_clock(finish)
    elapsed = (dble(finish - start)/dble(timeres))
    nruns = getnruns(elapsed)
    timeloop : select case (nruns)
    case (1)
       mean = elapsed
    case default
       call system_clock(start, timeres)
       floop : do i = 1,nruns
          out = f(arg1)
       end do floop
       call system_clock(finish)
       mean = (dble(finish - start)/(dble(timeres)*dble(nruns)))
    end select timeloop
    write(*,"(A60)") "------------------------------------------------------------"
    write(*,"(A,I0,A,E9.3E2,A)") " Mean runtime (", nruns, " runs) = ", mean, " seconds"
    write(*,"(A)") " Result: "
    write(*,"(I30)") out
    write(*,"(A60)") "------------------------------------------------------------"
  end subroutine timei2i8

  subroutine timei2i2(f, arg1)
    implicit none
    interface
       function f(arg1) result (out)
         implicit none
         integer(kind=2), intent(in) :: arg1
         integer(kind=2) :: out
       end function f
    end interface
    integer(2) :: arg1
    integer(2) :: out
    integer(kind=8) :: start, finish, timeres
    integer(kind=8) :: nruns, i
    real(kind=8) :: elapsed, mean
    call system_clock(start, timeres)
    out = f(arg1)
    call system_clock(finish)
    elapsed = (dble(finish - start)/dble(timeres))
    nruns = getnruns(elapsed)
    timeloop : select case (nruns)
    case (1)
       mean = elapsed
    case default
       call system_clock(start, timeres)
       floop : do i = 1,nruns
          out = f(arg1)
       end do floop
       call system_clock(finish)
       mean = (dble(finish - start)/(dble(timeres)*dble(nruns)))
    end select timeloop
    write(*,"(A60)") "------------------------------------------------------------"
    write(*,"(A,I0,A,E9.3E2,A)") " Mean runtime (", nruns, " runs) = ", mean, " seconds"
    write(*,"(A)") " Result: "
    write(*,"(I30)") out
    write(*,"(A60)") "------------------------------------------------------------"
  end subroutine timei2i2

end module timeitmodule
