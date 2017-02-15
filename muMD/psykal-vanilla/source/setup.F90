!    Copyright (c) 2016-2016 Alin Marin Elena <alinm.elena@gmail.com>
!    The MIT License http://opensource.org/licenses/MIT

module m_setup
  use m_constants
  use m_Types
  use m_Useful
#ifdef __GFORTRAN__
  use iso_fortran_env, only : OUTPUT_UNIT,compiler_options,compiler_version
#else
  use iso_fortran_env, only : OUTPUT_UNIT
#endif
  implicit none

  private
  public :: setIO
  public :: closeIO
  public :: compilerInfo
  contains

  subroutine setIO(io)
    character (len=*), parameter :: myname = 'setIO'
    type (ioType), intent (inout) :: io

    integer :: errno

    if (io%stdout) then
      io%uout=OUTPUT_UNIT
    else
      open (newunit=io%uout, file=trim(io%outputFile), status="unknown", action="write", iostat=errno)
      if (errno /= 0) then
        call error(__FILE__,&
          __LINE__,&
          -107,"Cannot open "//trim(io%outputFile)//" file",io)
      endif
    endif
    open (newunit=io%uerr, file=trim(io%errFile), status="unknown", action="write", iostat=errno)
    if (errno /= 0) then
      call error(__FILE__,__LINE__,-108,"Cannot open "//trim(io%errFile)//" file",io)
    endif
    if (io%isTraj) then
      open (newunit=io%utraj, file=trim(io%trajectoryFile), status="unknown", action="write", iostat=errno)
      if (errno /= 0) then
        call error(__FILE__,&
          __LINE__&
          ,-108,"Cannot open "//trim(io%trajectoryFile)//" file",io)
      endif
    endif

  end subroutine setIO

  subroutine compilerInfo(io)
    character (len=*), parameter :: myname = 'compilerInfo'
    type (ioType), intent (inout) :: io
    integer                :: l 
#ifdef __GFORTRAN__
    character(len=k_ml*10) :: m,copt=compiler_options()
    write(io%uout,'(5a)') 'This file was compiled by ', &
                    compiler_version(), ' using the options ', trim(copt)
#endif   
  end subroutine compilerInfo


  subroutine closeIO(io)
    character (len=*), parameter :: myname = 'closeIO'
    type (ioType), intent (inout) :: io

    if (io%isTraj) then
      close(io%utraj)
    endif
  end subroutine closeIO
end module m_setup
