program main
  use probs
  use utilsmodule, only : timeit, eratosthenes, plim
  implicit none
  integer(kind=1) :: res_i1 = 0
  integer(kind=2) :: res_i2 = 0
  integer(kind=4) :: res_i4 = 0, n = 0, i
  integer(kind=8) :: res_i8 = 0
  integer(kind=16) :: res_i16 = 0
  integer(kind=16) :: start, finish, timeres
  character(len=4) :: nchar
  real(kind=16) :: elapsed

!   NOTES ON INT SIZES
!   huge(i1) = 127
!   huge(i2) = 32767
!   huge(i4) = 2147483647
!   huge(i8) = 9223372036854775807
!   huge(i16) = 170141183460469231731687303715884105727

  write(*,*) res_i4
  ioloop : do
     n = 0
     write(*,*)
     write(*,*) "Enter problem number: "
     write(6,"(A5)",advance="no") " >>> "
     read(*,*) nchar
     write(*,*)
     choose_prob : select case (nchar(1:2))
     case ("q", "Q")
        exit ioloop
     case ("0":"9")
        read(nchar,*) n
     case default
        stop
     end select choose_prob
     run_prob : select case (n)
     case (0)
        stop
     case (1)
        call timeit(p01)
     case (2)
        call timeit(p02)
     case (3)
        call timeit(p03)
     case (4)
        call timeit(p04)
     case (5)
        call timeit(p05)
     case (6)
        call timeit(p06)
     case (7)
        call timeit(p07)
     case (8)
        call timeit(p08)
     case (9)
        call timeit(p09)
     case (10)
        call timeit(p10)
     case (11)
        call timeit(p11)
     case (12)
        call timeit(p12)
     case (13)
        call timeit(p13)
     case (14)
        call timeit(p14,int(1000000, 8))
     case (15)
        call timeit(p15,int(20,2))
     case (16)
        call timeit(p16,1000)
     case (17)
        call timeit(p17)
     case (18)
        call timeit(p18)
     case (19)
        call timeit(p19)
     case (20)
        call timeit(p20,int(100,2))
     case default
        stop
     end select run_prob
  end do ioloop

end program main
