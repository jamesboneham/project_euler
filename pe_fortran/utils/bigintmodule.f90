module bigintmodule
  use mpmodule
  implicit none
  private
  public :: big_of_int
  interface big_of_int
     module procedure big_i1
     module procedure big_i2
     module procedure big_i4
     module procedure big_i8
     module procedure big_i16
  end interface big_of_int
contains
  type (mp_real) function big_i1(x)
    integer(1) :: x
    character(4) :: buffer
    write(buffer,"(I4)") x
    big_i1 = mpreal(trim(buffer)//".")
  end function big_i1
  type (mp_real) function big_i2(x)
    integer(2) :: x
    character(6) :: buffer
    write(buffer,"(I6)") x
    big_i2 = mpreal(trim(buffer)//".")
  end function big_i2
  type (mp_real) function big_i4(x)
    integer(4) :: x
    character(11) :: buffer
    write(buffer,"(I11)") x
    big_i4 = mpreal(trim(buffer)//".")
  end function big_i4
  type (mp_real) function big_i8(x)
    integer(8) :: x
    character(20) :: buffer
    write(buffer,"(I20)") x
    big_i8 = mpreal(trim(buffer)//".")
  end function big_i8
  type (mp_real) function big_i16(x)
    integer(16) :: x
    character(40) :: buffer
    write(buffer,"(I40)") x
    big_i16 = mpreal(trim(buffer)//".")
  end function big_i16

end module bigintmodule
