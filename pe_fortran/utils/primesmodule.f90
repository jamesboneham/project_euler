module primesmodule
  implicit none
  private
  public :: eratosthenes, plim
  integer(2), parameter :: i1=1, i2=2, i4=4, i8=8, i16=16
  interface plim
     module procedure plimi4
  end interface plim
  interface eratosthenes
     module procedure eratosthenesi4
  end interface eratosthenes
contains

  subroutine eratosthenesi4(nmax, primesize, np, primes)
    implicit none
    integer(i4), intent(in) :: nmax, primesize
    integer(i4), intent(out) :: np
    integer(i4), dimension(primesize), intent(out) :: primes
    integer(i4) :: i, j, k, ilim
    logical, dimension(2:nmax) :: mask
    ilim = int(sqrt(real(nmax,8)+0.5),i4)
    mask(:) = .true.
    k = 0
    do i = 2,ilim
       if (mask(i)) then
          k = k + 1
          primes(k) = i
          do j = i*i, nmax, i
             mask(j) = .false.
          end do
       end if
    end do
    do i = (ilim+1), nmax
       if (mask(i)) then
          k = k+1
          primes(k) = i
       end if
    end do
    np = k
  end subroutine eratosthenesi4

  integer(i4) pure function plimi4(nmax)
    implicit none
    integer(i4), intent(in) :: nmax
    real(i8) :: x
    x = real(nmax, i8)
    plimi4 = int((30.*x*log(113.))/(113.*log(x)), i4)
  end function plimi4


end module primesmodule
