!    Copyright (c) 2016-2016 Alin Marin Elena <alinm.elena@gmail.com>
!    The MIT License http://opensource.org/licenses/MIT

!> \brief subprograms of general use
!> \author Alin M. Elena 
!> \date 14-15th of January, 2006
!> \remark August, 2016
module m_Useful
  use m_Constants
  use m_Types
  use iso_fortran_env, only : ERROR_UNIT
  !
  implicit none
  private
  integer(ip), public        :: ibuff(10000)
  public :: cstr
  public :: error
  public :: DateAndTime
  public :: invertH  !
  public :: getVdw
  public :: hs

contains


  pure integer function getVdw(i,j,n)
    integer, intent(in) :: i,j,n
    getVdw=merge(i,n*i-i*(i-1)/2+(j-i),i==j)
  end function getVdw


  !> \brief logical function compares two strings
  !> \details not case sensitive returns true if the strings are the same false otherwise
  !> \author Alin M. Elena (Queen's University Belfast)
  !> \date 14-15th of January, 2006
  !> \param str1, str2 character(len=*) that get compared
  !
  logical function cstr (str1, str2)
    character (len=*), parameter :: myname = 'cstr'
    character (len=*) :: str1, str2
    integer :: len1, len2, i, s
    !
    len1 = len (str1)
    len2 = len (str2)
    cstr = .false.
    if (len1 == len2) then
      s = 0
      do i = 1, len1
        s = s + Abs (up(str1(i:i))-up(str2(i:i)))
      end do
      if (s == 0) cstr = .true.
    end if
  end function cstr
  !
  !> \brief integer function returns the ascii code of a letter
  !> \details always will be the ascii code of the capital letter
  !> as long as the characters are grouped in the set consecutive it should work
  !> (A-Z a-z)
  !> \author Alin M. Elena (Queen's University Belfast)
  !> \date 14-15th of January, 2006
  !> \param a character
  integer function up (a)
    character (len=1) :: a
    if (iachar(a) > iachar("Z")) then
      up = iachar (a) - (iachar("a")-iachar("A"))
    else
      up = iachar (a)
    end if
  end function up

  subroutine error(filename,line,code,message,ioLoc) 
    integer, intent(in)          :: line, code
    character(len=*), intent(in) :: filename, message
    type (ioType), intent (in) :: ioLoc
    write(ERROR_UNIT,*)"Error!!! check the file "//trim(ioLoc%errFile)//" for details"
    write(ioLoc%uerr,'(a,i0)')"error in file "//trim(filename)//" at line ",line
    write(ioLoc%uerr,'(a)')trim(message)
    write(ioLoc%uerr,'(a,i0)')"Error code: ",code
    stop 
  end subroutine error

  !
  !> \brief return the date and time in a human format
  !> \author Alin M Elena
  !> \date 31/10/07, 09:48:57
  !> \param date character contains the date in the format dd-mm-yyyy
  !> \param time character contains the time in the format hh:mm:ss.mmmgoo
  subroutine DateAndTime (date, time)
    character (len=*), parameter :: sMyName = "DateAndTime"
    character (len=*), intent (out) :: date, time
    character (len=8) :: dt
    character (len=10) :: tm
    !
    call date_and_time (dt, tm)
    date (7:10) = dt (1:4)
    date (3:3) = "-"
    date (4:5) = dt (5:6)
    date (6:6) = "-"
    date (1:2) = dt (7:8)
    time (1:2) = tm (1:2)
    time (3:3) = ":"
    time (4:5) = tm (3:4)
    time (6:6) = ":"
    time (7:12) = tm (5:10)
  end subroutine DateAndTime

  pure real(kind=rp) function determinant(a)
    real(kind=rp), intent(in)  :: a(3,3)

    determinant = a(1,1)*(a(3,3)*a(2,2)-a(3,2)*a(2,3)) &
      - a(2,1)*(a(3,3)*a(1,2)-a(3,2)*a(1,3)) &
      + a(3,1)*(a(2,3)*a(1,2)-a(2,2)*a(1,3))
  end function determinant

  pure function invertH(a) result(b)
    real(kind=rp), intent(in)  :: a(3,3)
    real(kind=rp)  :: b(3,3)

    real(kind=rp) :: idet

    idet=1.0_rp/determinant(a)
    b(1,1)= idet*(a(3,3)*a(2,2)-a(3,2)*a(2,3))
    b(1,2)= idet*(a(3,1)*a(2,3)-a(3,3)*a(2,1))
    b(1,3)= idet*(a(3,2)*a(2,1)-a(3,1)*a(2,2))

    b(2,1)= idet*(a(3,2)*a(1,3)-a(3,3)*a(1,2))
    b(2,2)= idet*(a(3,3)*a(1,1)-a(3,1)*a(1,3))
    b(2,3)= idet*(a(3,1)*a(1,2)-a(3,2)*a(1,1))

    b(3,1)= idet*(a(2,3)*a(1,2)-a(2,2)*a(1,3))
    b(3,2)= idet*(a(2,1)*a(1,3)-a(2,3)*a(1,1))
    b(3,3)= idet*(a(2,2)*a(1,1)-a(2,1)*a(1,2))
  end function invertH

  pure function hs(h,r) result(s)
    real(rp),intent(in) :: h(3,3),r(3)
    real(rp) :: s(3)

    s(1)=h(1,1)*r(1)+h(1,2)*r(2)+h(1,3)*r(3)
    s(2)=h(2,1)*r(1)+h(2,2)*r(2)+h(2,3)*r(3)
    s(3)=h(3,1)*r(1)+h(3,2)*r(2)+h(3,3)*r(3)

  end function hs

end module m_Useful
