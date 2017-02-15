!    Copyright (c) 2016-2016 Alin Marin Elena <alinm.elena@gmail.com>
!    The MIT License http://opensource.org/licenses/MIT

!> \brief deals with reading the field file(s)
!> \details it saves the gathered info in the right variables
!> \author Alin M. Elena (Daresbury Laboratory)
!> \date 11th of April 2016
!> \remarks
!
module m_ReadField
  use m_Constants
  use m_Parser
  use m_Types
  use m_Useful, only : error,getVdw
  use m_units
  implicit none
  private
  public :: readField
  !
contains
  !
  !

  subroutine readPairs(io,ps)
    character (len=*), parameter         :: myname = 'readPairs'
    type(ioType), intent (inout)         :: io
    type(particlesType), intent(inout)   :: ps

    integer :: ub
    if(GetBlock(io,"pairs",ub)) then
      block 
        integer :: i,j,k
        character(len=k_ml) :: line
        character(len=elLen) :: a,b
        character(len=k_mw) :: pot
        integer :: ierr
        real(rp) :: params(8)

        do while ( GetLine(ub, line) )
          read(line,*,iostat=ierr)a,b,pot,params
          if (.not.isInSet(a,ps%species,ps%nSpecies,i)) then
            call error(__FILE__,__LINE__,-121,&
              "error unknown specie "//trim(a),io)
          endif
          if (.not.isInSet(b,ps%species,ps%nSpecies,j)) then
            call error(__FILE__,__LINE__,-122,&
              "error unknown specie "//trim(b),io)
          endif
          k=getVdw(i,j,ps%nSpecies)
          if(ps%vdw(k)%on) then
            call error(__FILE__,__LINE__,-119,&
              "pair already read: "//trim(a)//" and "//trim(b),io)
          endif 
          if (trim(pot)=='lj') then
            ps%vdw(k)%i = i 
            ps%vdw(k)%j = j 
            ps%vdw(k)%id = 1
            ps%vdw(k)%np=2
            ps%vdw(k)%on=.true.
            ps%vdw(k)%pot=trim(pot)
            ps%vdw(k)%fpot='Lennard-Jones ε,σ'
            allocate(ps%vdw(k)%param(2))
            ps%vdw(k)%param(1:2)=params(1:2)
          elseif (trim(pot)=='ljs') then
            ps%vdw(k)%i = i 
            ps%vdw(k)%j = j 
            ps%vdw(k)%id = 2
            ps%vdw(k)%np=2
            ps%vdw(k)%on=.true.
            ps%vdw(k)%pot=trim(pot)
            ps%vdw(k)%fpot='Lennard-Jones ε,σ'
            allocate(ps%vdw(k)%param(2))
            ps%vdw(k)%param(1:2)=params(1:2)
          else 
            call error(__FILE__,__LINE__,-123,&
              "unknown potential "//trim(pot),io)
          endif
        enddo
      end block
      close(ub,status="delete")
    else
      call error(__FILE__,__LINE__,-115,"Cannot read block pairs",io)
    endif 

  end subroutine readPairs

  subroutine readSpecies(io,ps)
    character (len=*), parameter         :: myname = 'readSpecies'
    type(ioType), intent (inout)         :: io
    type(particlesType), intent(inout)   :: ps

    integer :: ub

    if(GetBlock(io,"species",ub)) then
      block 
        integer :: i,j
        character(len=elLen) :: el
        real(rp) :: m,c,col
        integer :: ierr
        logical :: specs(ps%nSpecies)

        specs=.true.
        do i=1,ps%nSpecies
          read(ub,*,iostat=ierr)el,m,c,col
          if (ierr/=0) then 
            call error(__FILE__,__LINE__,-112,&
              "Error reading data for specie "//trim(ps%species(i)),io)
          endif 
          if (isInSet(el,ps%species,ps%nSpecies,j)) then
            if (specs(j)) then 
              specs(j)=.false.
            else
              call error(__FILE__,__LINE__,-114,&
                "Specie "//trim(el)//" already read",io)
            endif
            ps%mass(j)=m;ps%charge(j)=c
            ps%colour(j)=col
          else
            call error(__FILE__,__LINE__,-112,&
              "Specie "//trim(el)//" not found!!!",io)
          endif 
        enddo
      end block
    close(ub,status="delete")
    else
      call error(__FILE__,__LINE__,-111,"Cannot read block species",io)
    endif 

  end subroutine readSpecies

  subroutine readField(io,field,ps)
    character (len=*), parameter :: myname = 'readField'
    type (ioType), intent (inout) :: io
    type (fieldType), intent (inout) :: field
    type(particlesType), intent(inout) :: ps

    real (rp) :: aux
    integer :: errno = - 1
    character(len=k_ml) :: dummy
    !
    open (newunit=io%ufield, file=trim(io%fieldFile), status='old', action='read', iostat=errno)
    if (errno /= 0) then
      call error(__FILE__,&
        __LINE__,&
        -110,"Cannot open "//trim(io%fieldFile)//" file",io)
    end if
    ! parse file and in the same time
    ! makes a minimal check of corectness
    call ParseFile(io,io%ufield,42)

    dummy=getString(io,"title","no title",.true.)
    field%units=getString(io,"units","kcal/mol",.true.)   
    call setEnergyUnits(field)
    call readSpecies(io,ps)
    call readPairs(io,ps)

    dummy=getString(io,"finish","",.true.)
    call EndParse(io)
  end subroutine readField
end module m_ReadField
