module probs11to20
  use mpmodule
  use utilsmodule
  implicit none
  private
  integer(2), parameter :: i1=1, i2=2, i4=4, i8=8, i16=16
  !   NOTES ON INT SIZES
  !   huge(i1) = 127
  !   huge(i2) = 32767
  !   huge(i4) = 2147483647
  !   huge(i8) = 9223372036854775807
  !   huge(i16) = 170141183460469231731687303715884105727

  public :: p11, p12, p13, p14, p15, p16, p17, p18, p19, p20

contains

  integer(kind=i4) function p11()
    implicit none
    integer(kind=i4), dimension(-2:23,-2:23) :: grid
    integer(kind=i4) :: i, j, k, maxprod, prod
    p11 = 0
    maxprod = 0
    grid(:,:) = 0
    grid(1,1:20) = (/08, 02, 22, 97, 38, 15, 00, 40, 00, 75, 04, 05, 07, 78, &
         52, 12, 50, 77, 91, 08/)
    grid(2,1:20) = (/49, 49, 99, 40, 17, 81, 18, 57, 60, 87, 17, 40, 98, 43, &
         69, 48, 04, 56, 62, 00/)
    grid(3,1:20) = (/81, 49, 31, 73, 55, 79, 14, 29, 93, 71, 40, 67, 53, 88, &
         30, 03, 49, 13, 36, 65/)
    grid(4,1:20) = (/52, 70, 95, 23, 04, 60, 11, 42, 69, 24, 68, 56, 01, 32, &
         56, 71, 37, 02, 36, 91/)
    grid(5,1:20) = (/22, 31, 16, 71, 51, 67, 63, 89, 41, 92, 36, 54, 22, 40, &
         40, 28, 66, 33, 13, 80/)
    grid(6,1:20) = (/24, 47, 32, 60, 99, 03, 45, 02, 44, 75, 33, 53, 78, 36, &
         84, 20, 35, 17, 12, 50/)
    grid(7,1:20) = (/32, 98, 81, 28, 64, 23, 67, 10, 26, 38, 40, 67, 59, 54, &
         70, 66, 18, 38, 64, 70/)
    grid(8,1:20) = (/67, 26, 20, 68, 02, 62, 12, 20, 95, 63, 94, 39, 63, 08, &
         40, 91, 66, 49, 94, 21/)
    grid(9,1:20) = (/24, 55, 58, 05, 66, 73, 99, 26, 97, 17, 78, 78, 96, 83, &
         14, 88, 34, 89, 63, 72/)
    grid(10,1:20) = (/21, 36, 23, 09, 75, 00, 76, 44, 20, 45, 35, 14, 00, 61, &
         33, 97, 34, 31, 33, 95/)
    grid(11,1:20) = (/78, 17, 53, 28, 22, 75, 31, 67, 15, 94, 03, 80, 04, 62, &
         16, 14, 09, 53, 56, 92/)
    grid(12,1:20) = (/16, 39, 05, 42, 96, 35, 31, 47, 55, 58, 88, 24, 00, 17, &
         54, 24, 36, 29, 85, 57/)
    grid(13,1:20) = (/86, 56, 00, 48, 35, 71, 89, 07, 05, 44, 44, 37, 44, 60, &
         21, 58, 51, 54, 17, 58/)
    grid(14,1:20) = (/19, 80, 81, 68, 05, 94, 47, 69, 28, 73, 92, 13, 86, 52, &
         17, 77, 04, 89, 55, 40/)
    grid(15,1:20) = (/04, 52, 08, 83, 97, 35, 99, 16, 07, 97, 57, 32, 16, 26, &
         26, 79, 33, 27, 98, 66/)
    grid(16,1:20) = (/88, 36, 68, 87, 57, 62, 20, 72, 03, 46, 33, 67, 46, 55, &
         12, 32, 63, 93, 53, 69/)
    grid(17,1:20) = (/04, 42, 16, 73, 38, 25, 39, 11, 24, 94, 72, 18, 08, 46, &
         29, 32, 40, 62, 76, 36/)
    grid(18,1:20) = (/20, 69, 36, 41, 72, 30, 23, 88, 34, 62, 99, 69, 82, 67, &
         59, 85, 74, 04, 36, 16/)
    grid(19,1:20) = (/20, 73, 35, 29, 78, 31, 90, 01, 74, 31, 49, 71, 48, 86, &
         81, 16, 23, 57, 05, 54/)
    grid(20,1:20) = (/01, 70, 54, 71, 83, 51, 54, 69, 16, 92, 33, 48, 61, 43, &
         52, 01, 89, 19, 67, 48/)
    jloop: do j=1,20
       iloop: do i=1,20
          prod = max(&
               max(product(grid(i, j:(j+3))),&
                   product((/(grid(i+k,j+k), k=0,3)/))),&
               max(product((/(grid(i+k,j-k), k=0,3)/)),&
                   product(grid(i:(i+3), j))))
          if (prod > maxprod) maxprod = prod
       end do iloop
    end do jloop
    p11 = maxprod
  end function p11

  integer(kind=i4) function p12()
    implicit none
    integer(kind=i4), parameter :: nmax = 1000, nfacs = 500
    logical(kind=1), dimension(2:nmax) :: mask
    integer(kind=i4), allocatable, dimension(:) :: primes
    integer(kind=i4) :: i, j, p, plim, pmax, jmax, n1, n2, n0, nf, ai
    p12 = 0
    jmax = floor(sqrt(real(nmax)), kind=i4)
    plim = max(55, int(real(nmax)/(log(real(nmax))-4.)))
    mask = .true.
    p = 0
    allocate(primes(1:plim))
    primeloop : do i=2,nmax
       if (mask(i)) then
             p = p + 1
             primes(p) = i
             if (i < jmax) then
                sieve : do j=i*i,nmax,i
                   mask(j) = .false.
                end do sieve
             else
                cycle primeloop
             end if
       end if
    end do primeloop
    pmax = p
    n0 = 3
    trifact: do
       if (mod(n0,2) == 0) then
          n1 = n0/2
          n2 = n0+1
       else
          n1 = (n0+1)/2
          n2 = n0
       end if
       nf = 1
       factorize: do i=1,pmax
          p = primes(i)
          ai = 1
          do while (mod(n1,p)==0)
             n1 = n1/p
             ai = ai + 1
          end do
          do while (mod(n2,p)==0)
             n2 = n2/p
             ai = ai + 1
          end do
          nf = nf*ai
          if ((n1==1).and.(n2==1)) exit factorize
       end do factorize
       if (nf > nfacs) then
          exit trifact
       else
          n0 = n0 + 1
          cycle
       end if
    end do trifact
    deallocate(primes)
    p12 = ((n0*(n0+1))/2)
  end function p12

  integer(kind=i8) function p13()
    implicit none
    integer(kind=i4) :: i, j
    character(len=50*100) :: p13nums
    character(1), dimension(60) :: p13out
    character(10) :: p13outstr
    type (mp_real) :: p13accumulator
    p13nums = "37107287533902102798797998220837590246510135740250&
         &46376937677490009712648124896970078050417018260538&
         &74324986199524741059474233309513058123726617309629&
         &91942213363574161572522430563301811072406154908250&
         &23067588207539346171171980310421047513778063246676&
         &89261670696623633820136378418383684178734361726757&
         &28112879812849979408065481931592621691275889832738&
         &44274228917432520321923589422876796487670272189318&
         &47451445736001306439091167216856844588711603153276&
         &70386486105843025439939619828917593665686757934951&
         &62176457141856560629502157223196586755079324193331&
         &64906352462741904929101432445813822663347944758178&
         &92575867718337217661963751590579239728245598838407&
         &58203565325359399008402633568948830189458628227828&
         &80181199384826282014278194139940567587151170094390&
         &35398664372827112653829987240784473053190104293586&
         &86515506006295864861532075273371959191420517255829&
         &71693888707715466499115593487603532921714970056938&
         &54370070576826684624621495650076471787294438377604&
         &53282654108756828443191190634694037855217779295145&
         &36123272525000296071075082563815656710885258350721&
         &45876576172410976447339110607218265236877223636045&
         &17423706905851860660448207621209813287860733969412&
         &81142660418086830619328460811191061556940512689692&
         &51934325451728388641918047049293215058642563049483&
         &62467221648435076201727918039944693004732956340691&
         &15732444386908125794514089057706229429197107928209&
         &55037687525678773091862540744969844508330393682126&
         &18336384825330154686196124348767681297534375946515&
         &80386287592878490201521685554828717201219257766954&
         &78182833757993103614740356856449095527097864797581&
         &16726320100436897842553539920931837441497806860984&
         &48403098129077791799088218795327364475675590848030&
         &87086987551392711854517078544161852424320693150332&
         &59959406895756536782107074926966537676326235447210&
         &69793950679652694742597709739166693763042633987085&
         &41052684708299085211399427365734116182760315001271&
         &65378607361501080857009149939512557028198746004375&
         &35829035317434717326932123578154982629742552737307&
         &94953759765105305946966067683156574377167401875275&
         &88902802571733229619176668713819931811048770190271&
         &25267680276078003013678680992525463401061632866526&
         &36270218540497705585629946580636237993140746255962&
         &24074486908231174977792365466257246923322810917141&
         &91430288197103288597806669760892938638285025333403&
         &34413065578016127815921815005561868836468420090470&
         &23053081172816430487623791969842487255036638784583&
         &11487696932154902810424020138335124462181441773470&
         &63783299490636259666498587618221225225512486764533&
         &67720186971698544312419572409913959008952310058822&
         &95548255300263520781532296796249481641953868218774&
         &76085327132285723110424803456124867697064507995236&
         &37774242535411291684276865538926205024910326572967&
         &23701913275725675285653248258265463092207058596522&
         &29798860272258331913126375147341994889534765745501&
         &18495701454879288984856827726077713721403798879715&
         &38298203783031473527721580348144513491373226651381&
         &34829543829199918180278916522431027392251122869539&
         &40957953066405232632538044100059654939159879593635&
         &29746152185502371307642255121183693803580388584903&
         &41698116222072977186158236678424689157993532961922&
         &62467957194401269043877107275048102390895523597457&
         &23189706772547915061505504953922979530901129967519&
         &86188088225875314529584099251203829009407770775672&
         &11306739708304724483816533873502340845647058077308&
         &82959174767140363198008187129011875491310547126581&
         &97623331044818386269515456334926366572897563400500&
         &42846280183517070527831839425882145521227251250327&
         &55121603546981200581762165212827652751691296897789&
         &32238195734329339946437501907836945765883352399886&
         &75506164965184775180738168837861091527357929701337&
         &62177842752192623401942399639168044983993173312731&
         &32924185707147349566916674687634660915035914677504&
         &99518671430235219628894890102423325116913619626622&
         &73267460800591547471830798392868535206946944540724&
         &76841822524674417161514036427982273348055556214818&
         &97142617910342598647204516893989422179826088076852&
         &87783646182799346313767754307809363333018982642090&
         &10848802521674670883215120185883543223812876952786&
         &71329612474782464538636993009049310363619763878039&
         &62184073572399794223406235393808339651327408011116&
         &66627891981488087797941876876144230030984490851411&
         &60661826293682836764744779239180335110989069790714&
         &85786944089552990653640447425576083659976645795096&
         &66024396409905389607120198219976047599490197230297&
         &64913982680032973156037120041377903785566085089252&
         &16730939319872750275468906903707539413042652315011&
         &94809377245048795150954100921645863754710598436791&
         &78639167021187492431995700641917969777599028300699&
         &15368713711936614952811305876380278410754449733078&
         &40789923115535562561142322423255033685442488917353&
         &44889911501440648020369068063960672322193204149535&
         &41503128880339536053299340368006977710650566631954&
         &81234880673210146739058568557934581403627822703280&
         &82616570773948327592232845941706525094512325230608&
         &22918802058777319719839450180888072429661980811197&
         &77158542502016545090413245809786882778948721859617&
         &72107838435069186155435662884062257473692284509516&
         &20849603980134001723930671666823555245252804609722&
         &53503534226472524250874054075591789781264330331690"
    p13accumulator = mpreal("0.")
    sumloop : do i=1,(50*100 - 49),50
       p13accumulator = p13accumulator + mpreal(p13nums(i:i+49)//".")
    end do sumloop
    call mpfform (p13accumulator, size(p13out), 0, p13out)
    j = 1
    digitloop : do i=1,size(p13out)
       if (j > 10) then
          exit digitloop
       end if
       if (p13out(i) == " ") then
          cycle
       end if
       p13outstr(j:j) = p13out(i)
       j = j+1
    end do digitloop
    read(p13outstr(1:10),'(I10)') p13
  end function p13

  integer(i4) function p14(nmax)
    implicit none
    integer(i8), intent(in) :: nmax
    integer(i8) :: n, x
    integer(i4), dimension(:), allocatable :: cache
    integer(i4) :: maxlen, imax
    allocate(cache(nmax))
    cache(:) = 0
    cache(1) = 1
    imax = nmax
    maxlen = 0
    do n = 2*(nmax/2)-1,(nmax/2),-2
       if (cache(n) == 0) then
          call p14_collatz(n, nmax, cache, x)
       else
          x = cache(n)
       end if
       if (x > maxlen) then
          imax = n
          maxlen = x
       end if
    end do
    deallocate(cache)
    p14 = imax
  end function p14

  recursive subroutine p14_collatz(n, nmax, cache, x)
    implicit none
    integer(i8), intent(in) :: n, nmax
    integer(i8), intent(inout) :: x
    integer(i4), dimension(:), intent(inout) :: cache
    if (n==1) then
       x = 1
    end if
    if (n <= nmax) then
       if (cache(n) > 0) then
          x = cache(n)
       else
          if (mod(n,2) == 0) then
             call p14_collatz(n/2, nmax, cache, x)
          else
             call p14_collatz(3*n + 1, nmax, cache, x)
          end if
          x = x + 1
          cache(n) = x
       end if
    else
       if (mod(n,2) == 0) then
          call p14_collatz(n/2, nmax, cache, x)
       else
          call p14_collatz(3*n + 1, nmax, cache, x)
       end if
       x = x + 1
    end if
  end subroutine p14_collatz

  integer(i8) function p15(sidelen)
    implicit none
    integer(i2), intent(in) :: sidelen
    integer(i2) :: i, j
    integer(i8), dimension(0:sidelen) :: buffer
    buffer(:) = 1
    do i=1,sidelen
       do j=1,sidelen
          buffer(j) = buffer(j) + buffer(j-1)
       end do
    end do
    p15 = buffer(sidelen)
  end function p15

  integer(i4) function p16(power)
    implicit none
    integer(i4), intent(in) :: power
    character(len=10) :: powerstr
    type (mp_real) :: two, pwr
    integer(i2), parameter :: buffersize=400
    integer(i2) :: i, d
    character(1), dimension(buffersize) :: chararr
    character(1) :: dchar
    write(powerstr, "(I0)") power
    two = mpreal("2.")
    pwr = mpreal(trim(powerstr)//".")
    chararr(:) = "0"
    call mpfform (two**pwr, size(chararr), 0, chararr)
    p16 = 0
    sumloop : do i=1, buffersize
       dchar(1:1) = chararr(i)
       readchar : select case (dchar)
       case (" ")
          cycle sumloop
       case (".")
          exit sumloop
       case default
          read(dchar,"(I1)") d
          p16 = p16 + d
          continue
       end select readchar
    end do sumloop
  end function p16

  integer(i2) function p17()
    integer(i2), parameter :: l1=3, l2=3, l3=5, l4=4, l5=4, l6=3, l7=5, l8=5, l9=4, &
         l10=3, l11=6, l12=6, l13=8, l14=8, l15=7, l16=7, l17=9, l18=8, l19=8, &
         l20=6, l30=6, l40=5, l50=5, l60=5, l70=7, l80=6, l90=6, &
         l100=7, l1000=8, land=3
    p17 = (90*(l1 + l2 + l3 + l4 + l5 + l6 + l7 + l8 + l9) &
         + 10*(l10 + l11 + l12 + l13 + l14 + l15 + l16 + l17 + l18 + l19) &
         + 100*(l20 + l30 + l40 + l50 + l60 + l70 + l80 + l90) &
         + 100*(l1 + l2 + l3 + l4 + l5 + l6 + l7 + l8 + l9 + 9*l100) &
         + (l1 + l1000) + (9*99)*(land))
  end function p17

  integer(i4) function p18()
    integer(i2), parameter :: nrows = 15, one=1
    integer(i2), parameter :: arrlen = (nrows*(nrows+1))/2
    integer(i2) :: i, j, k0, k1
    integer(i2), dimension(arrlen) :: tri
    tri = (/ 75, &
         95, 64, &
         17, 47, 82, &
         18, 35, 87, 10, &
         20, 04, 82, 47, 65, &
         19, 01, 23, 75, 03, 34, &
         88, 02, 77, 73, 07, 63, 67, &
         99, 65, 04, 28, 06, 16, 70, 92, &
         41, 41, 26, 56, 83, 40, 80, 70, 33, &
         41, 48, 72, 33, 47, 32, 37, 16, 94, 29, &
         53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14, &
         70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57, &
         91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48, &
         63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31, &
         04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23 /)
    do i = 14,1,-1
       do j = 1,i
          k0 = k(i,j)
          k1 = k(i+one,j)
          tri(k0) = tri(k0) + max(tri(k1), tri(k1+one))
       end do
    end do
    p18 = tri(1)
  contains
    integer(i2) function k(i,j)
      implicit none
      integer(i2) :: i,j
      k = ((i-1)*i)/2 + j
    end function k
  end function p18

  integer(i2) function p19()
    integer(i2) :: dow, dom, dim, mnth, yr
    logical :: ly
    dow=1
    dom=1
    dim=31
    mnth=1
    yr=1900
    ly=.false.
    p19 = 0
    do while (yr < 1901)
       call step(dow, dom, dim, mnth, yr, ly)
    end do
    do while (yr < 2001)
       if ((dom==1).and.(dow==7)) then
          p19 = p19 + 1
       end if
       call step(dow, dom, dim, mnth, yr, ly)
    end do
  contains
    subroutine step(dow, dom, dim, mnth, yr, ly)
      integer(i2), parameter :: four=4, four100=400, zero=0
      integer(i2), intent(inout) :: dow, dom, dim, mnth, yr
      logical, intent(inout) :: ly
      nextmonth : if (dom >= dim) then
         select case (mnth)
         case (12)
            mnth = 1
            dim = daysinmonth(mnth, ly)
            yr = yr + 1
            if ((mod(yr, four)==zero).and.(mod(yr, four100)/=zero)) then
               ly = .true.
            else
               ly = .false.
            end if
         case default
            mnth = mnth + 1
            dim = daysinmonth(mnth, ly)
         end select
         dom = 1
      else
         dom = dom + 1
      end if nextmonth
      sunday : if (dow == 7) then
         dow = 1
      else
         dow = dow + 1
      end if sunday
    end subroutine step
    integer(i2) function daysinmonth(month, leapyear)
      integer(i2) :: month
      logical :: leapyear
      select case (month)
         case (4,6,9,11)
            daysinmonth = 30
         case (2)
            if (leapyear) then
               daysinmonth = 29
            else
               daysinmonth = 28
            end if
         case default
            daysinmonth = 31
      end select
    end function daysinmonth
  end function p19

  integer(i2) function p20(nmax)
    implicit none
    integer(i2), intent(in) :: nmax
    type (mp_real) :: n, fac, one
    integer(i2), parameter :: buffersize=200
    integer(i2) :: i, d
    character(1), dimension(buffersize) :: chararr
    character(1) :: dchar

    one = mpreal("1.")
    n = one
    fac = one
    do i=2,nmax
       n = n+one
       fac = fac*n
    end do
    call mpfform (fac, size(chararr), 2, chararr)
    p20 = 0
    sumloop : do i=1, buffersize
       dchar(1:1) = chararr(i)
       readchar : select case (dchar)
       case (" ")
          cycle sumloop
       case (".")
          exit sumloop
       case default
          read(dchar,"(I1)") d
          p20 = p20 + d
          continue
       end select readchar
    end do sumloop
  end function p20

end module probs11to20
