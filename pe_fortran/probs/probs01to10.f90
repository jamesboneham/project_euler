module probs01to10
  implicit none
  private

  integer(2), parameter :: i1=1, i2=2, i4=4, i8=8, i16=16
  !   NOTES ON INT SIZES
  !   huge(i1) = 127
  !   huge(i2) = 32767
  !   huge(i4) = 2147483647
  !   huge(i8) = 9223372036854775807
  !   huge(i16) = 170141183460469231731687303715884105727

  public :: p01, p02, p03, p04, p05, p06, p07, p08, p09, p10

contains

  integer(kind=i4) function p01()
    implicit none
    integer(kind=i2), parameter :: nmax = 1000
    integer(kind=i2) :: i
    p01 = 0
    loop : do i = 3, (nmax-1)
       ismult : if ((mod(i,3) == 0) .or. (mod(i,5) == 0)) then
          p01 = p01 + i
       else
          cycle
       end if ismult
    end do loop
  end function p01

  integer(kind=i4) function p02()
    implicit none
    integer(kind=i4) :: f1, f2, f1temp, f2temp
    f1 = 1
    f2 = 2
    p02 = 0
    oloop : do while (f2 < 4E6)
       p02 = p02 + f2
       f1temp = f1 + 2*f2
       f2temp = 2*f1 + 3*f2
       f1 = f1temp
       f2 = f2temp
    end do oloop
  end function p02

  integer(kind=i8) function p03()
    implicit none
    integer(kind=i8) :: nmax, factor, max_factor
    character(len=20) :: nmax_str = "600851475143"
    read(nmax_str,*) nmax
    max_factor = 1
    factor = 2
    hpfloop : do
       break_p : if (factor > nmax) then
          exit hpfloop
       else if (mod(nmax, factor) == 0) then
          nmax = nmax/factor
          max_factor = factor
       else
          if (factor == 2) then
             factor = 3
          else
             factor = factor + 2
          end if
       end if break_p
    end do hpfloop
    p03 = max_factor
  end function p03

  logical(kind=1) function isPalindrome(n)
    implicit none
    integer(kind=i4), intent(in) :: n
    integer(kind=i4) :: nrev, nfwd
    nfwd = n
    nrev = 0
    reverse : do while (nfwd > 0)
       nrev = 10*nrev + mod(nfwd, 10)
       nfwd = nfwd/10
    end do reverse
    isPalindrome = (n == nrev)
  end function isPalindrome

  integer(kind=i4) function p04()
    implicit none
    integer(kind=i4) :: a, b, ab
    p04 = 0
    aloop : do a=999,100,-1
       bloop : do b=990,100,-11
          ab = a*b
          if (ab < p04) then
             exit bloop
          else if (isPalindrome(ab)) then
             p04 = ab
             exit bloop
          end if
       end do bloop
    end do aloop
  end function p04

  integer(kind=i8) function p05()
    implicit none
    p05 = (2**4)*(3**2)*5*7*11*13*17*19
  end function p05

  integer(kind=i8) function p06()
    implicit none
    integer(kind=4) :: i
    integer(kind=i8) :: sq_sum, sum_sq
    sq_sum = 0
    sum_sq = 0
    do i=1,100
       sq_sum = sq_sum + i
       sum_sq = sum_sq + i*i
    end do
    sq_sum = sq_sum*sq_sum
    p06 = sq_sum - sum_sq
  end function p06

  integer(kind=i4) function p07()
    implicit none
    integer(kind=i4) :: nmax
    logical(kind=1), allocatable, dimension(:) :: mask
    integer(kind=i4) :: i, j, k, jmax
    nmax = 200000
    jmax = floor(sqrt(real(nmax)), kind=i4)
    allocate(mask(2:nmax))
    mask(2:) = .true.
    k = 1
    primeloop : do i=2,nmax
       if (mask(i)) then
          if (k == 10001) then
             p07 = i
             exit primeloop
          else
             k = k + 1
             if (i < jmax) then
                sieve : do j=i*i,nmax,i
                   mask(j) = .false.
                end do sieve
             else
                cycle primeloop
             end if
          end if
       end if
    end do primeloop
    deallocate(mask)
  end function p07

  integer(kind=i4) function p08()
    implicit none
    character(len=1000) :: digstr
    character(len=13) :: substr
    integer(kind=4) :: j
    integer(kind=4) :: arri
    integer(kind=2) :: i
    integer(kind=8) :: subprod, maxprod, prod13
    p08 = 1
    digstr = &
         "73167176531330624919225119674426574742355349194934&
         &96983520312774506326239578318016984801869478851843&
         &85861560789112949495459501737958331952853208805511&
         &12540698747158523863050715693290963295227443043557&
         &66896648950445244523161731856403098711121722383113&
         &62229893423380308135336276614282806444486645238749&
         &30358907296290491560440772390713810515859307960866&
         &70172427121883998797908792274921901699720888093776&
         &65727333001053367881220235421809751254540594752243&
         &52584907711670556013604839586446706324415722155397&
         &53697817977846174064955149290862569321978468622482&
         &83972241375657056057490261407972968652414535100474&
         &82166370484403199890008895243450658541227588666881&
         &16427171479924442928230863465674813919123162824586&
         &17866458359124566529476545682848912883142607690042&
         &24219022671055626321111109370544217506941658960408&
         &07198403850962455444362981230987879927244284909188&
         &84580156166097919133875499200524063689912560717606&
         &05886116467109405077541002256983155200055935729725&
         &71636269561882670428252483600823257530420752963450"
    maxprod = 0
    scan_main: do j=1,988
       substr = digstr(j:(j+12))
       subprod = 1
       readstr: do i=1,13
          read(substr(i:i),"(I1)") arri
          if (arri == 0) then
             subprod = 0
             exit readstr
          else
             subprod = subprod * arri
          end if
       end do readstr
       if (subprod > maxprod) maxprod = subprod
    end do scan_main
    p08 = maxprod
  end function p08

  integer(kind=4) function p09()
    integer(kind=4) :: a, b, c
    p09 = 1
    aloop: do a = 5,500,5
       bloop: do b = (500-a),500,1
          if (mod(a*b, 1000) /= 0) cycle bloop
          c = 1000 - a - b
          if (a*a + b*b - c*c == 0) then
             exit aloop
          end if
       end do bloop
    end do aloop
    p09 = a*b*c
  end function p09

  integer(kind=i8) function p10()
    implicit none
    integer(kind=i4), parameter :: nmax = 1999999, jlim = 1414
    logical(kind=1), dimension(2:nmax) :: mask
    integer(kind=i4) :: i, j
    integer(kind=i8) :: psum
    psum = 0
    mask(2:) = .true.
    primeloop : do i=2,nmax
       if (mask(i)) then
          psum = psum + i
          if (i > jlim) cycle primeloop
          sieve : do j=i*i,nmax,i
             mask(j) = .false.
          end do sieve
       end if
    end do primeloop
    p10 = psum
  end function p10

end module probs01to10
