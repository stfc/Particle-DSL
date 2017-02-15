!    Copyright (c) 2016-2016 Alin Marin Elena <alinm.elena@gmail.com>
!    The MIT License http://opensource.org/licenses/MIT

!> \brief deals with reading the input file(s)
!> \details it saves the gathered info in the right variables
!> \author Alin M. Elena (Daresbury Laboratory)
!> \date 11th of April 2016
!> \remarks
!
module m_ReadControl
  use m_Constants
  use m_Parser
  use m_Types
  use m_Setup, only : SetIO
  use m_Useful, only : error
  implicit none
  private
  public :: readControl
  !
contains
  !

  subroutine lerror(ou,errno,filename,errfile)
    integer, intent(in) :: ou, errno
    character(len=*),intent(in)    :: filename
    character(len=*),intent(in),optional    :: errfile

    if (errno /= 0) then
      if (ou/=0) then 
         write (0,*) "error check file "//trim (errfile)//" for details"
      endif 
      write (ou,*) "I can not open file ", trim (filename)
      write (ou,*) "Error no: ", errno
      stop
    end if

  end subroutine lerror 

  subroutine readControl(io,control)
    character (len=*), parameter :: myname = 'readControl'
    type (ioType), intent (inout) :: io
    type (controlType), intent (inout) :: control

    real (rp) :: aux
    integer :: errno = - 1,itmp
    character(len=k_ml) :: dummy
    !
    io%errFile = trim (io%controlFile) // ".err"
    open (newunit=io%uerr, file=trim(io%errFile), status="replace", action="write", iostat=errno)
    call lerror(0,errno,io%errFile)
    open (newunit=io%uinp, file=trim(io%controlFile), status='old', action='read', iostat=errno)
    call lerror(io%uerr,errno,io%controlFile,io%errFile)
    ! parse file and in the same time
    ! makes a minimal check of corectness
    call ParseFile(io,io%uinp)

    io%debugFile=getString(io,"debug","DEBUG",.false.)    
    open (newunit=io%udeb, file=trim(io%debugFile), status='replace', action='write', iostat=errno)
    call lerror(io%uerr,errno,io%controlFile,io%errFile)
    io%outputFile=getString(io,"output","OUTPUT",.true.)    
    io%trajectoryFile=getString(io,"trajname","HISTORY",.true.)    
    io%errFile=getString(io,"error","ERROR",.true.)    
    io%stdout=getLogical(io,"l_scr",.false.)    
    io%isTraj=getLogical(io,"trajectory",.false.)    
    close(io%uerr)
    call setIO(io)
    dummy=trim (io%controlFile) // ".err"
    open (newunit=itmp, file=trim(dummy), status='old', action='read', iostat=errno)
    if (errno /= 0) then
      call error(__FILE__,__LINE__,-109,"Cannot open "//trim(dummy)//" file",io)
    end if
    do while (GetLine(itmp, dummy))
      write(io%udeb,'(a)') trim(dummy)
    enddo
    close(itmp,status='delete')

    dummy=getString(io,"title","no title",.true.)
    io%statisFile=getString(io,"statis","STATIS",.true.)    
    io%configFile=getString(io,"config","CONFIG",.true.)    
    io%fieldFile =getString(io,"field","FIELD",.true.)    
    control%temperature=getReal(io,"temperature",298.0_rp)
    control%steps=getInteger(io,"steps",42)
    control%rc=getReal(io,"rcut",12.0_rp)
    control%lamda=getReal(io,"healing",1.0_rp)
    control%delta=getReal(io,"skin",3.5_rp)
    control%alpha=getReal(io,"alpha",0.35_rp)
    control%timestep=getReal(io,"timestep",0.001_rp)
    control%electrostatics=getLogical(io,"electrostatics",.true.)
    dummy=getString(io,"finish","",.true.)

    call EndParse(io)
    close(io%uinp)

  end subroutine readControl
end module m_ReadControl
