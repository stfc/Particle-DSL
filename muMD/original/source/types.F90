!    Copyright (c) 2016-2016 Alin Marin Elena <alinm.elena@gmail.com>
!    The MIT License http://opensource.org/licenses/MIT
module m_Types
  use m_Constants
  implicit none
  private

  type, public :: ioType
    character (len=k_mw) :: controlFile !<  name of the input file from which the data is read
!>  name of the error input file (parsing errors, before opening specific output files)
    character (len=k_mw) :: errFile
!>  name of the file in which the output is put
    character (len=k_mw) :: outputFile
!>   name of the file in which the debug data is written
    character (len=k_mw) :: debugFile
!>   name of the file in which the animation data is written
    character (len=k_mw) :: trajectoryFile="HISTORY"
    character (len=k_mw) :: statisFile
    character (len=k_mw) :: configFile
    character (len=k_mw) :: fieldFile
!>  how much information to be Printed in the output file
    integer :: verbosity
!>  the debug level
    integer :: debug
!>  output on screen?
    logical :: stdout
!>  do we have trajectory
    logical :: istraj
!>  unit number for input file
    integer :: uinp
!>  unit number for output file
    integer :: uout
!>  unit number for debug file
    integer :: udeb = - 1
!>  unit number for error file
    integer :: uerr
!>  unit number for animation file
    integer :: utraj
    integer :: ufield
    integer :: ustatis
    integer :: uconfig
!> is it first time when is read?
    logical :: firstTime = .false.
  end type ioType

  type, public :: neighType
    integer :: n=0
    integer, allocatable :: list(:)
  end type neighType

  type, public :: vdwType
    integer :: i=0,j=0,id=0,np=0
    character(len=k_mw) :: pot=''
    character(len=k_mw) :: fpot=''
    logical :: on=.false.
    real(rp),allocatable :: param(:)
  end type vdwType

  type, public :: engStressType
    real(rp)                          :: engPair=0.0_rp
    real(rp)                          :: engElec=0.0_rp
    real(rp)                          :: stress(3,3)=0.0_rp
  end type engStressType

  type,public :: particlesType 
    character(len=k_ml)               :: systemName
    integer                           :: nGParticles=0
    integer(ip), allocatable          :: GIDs(:),spec(:)
    real(rp), allocatable             ::  x(:),  y(:),  z(:)
    real(rp), allocatable             :: vx(:), vy(:), vz(:)
    real(rp), allocatable             :: fx(:), fy(:), fz(:)
    real(rp), allocatable             :: fxo(:), fyo(:), fzo(:)
    real(rp)                          :: h(3,3) = 0.0_rp
    real(rp)                          :: hi(3,3) = 0.0_rp
    integer                           :: pbc,nSpecies
    character(len=elLen), allocatable :: labels(:)  
    character(len=elLen), allocatable :: species(:) ! unique set of species available  
    real(rp),allocatable              :: mass(:) ! mass of species
    real(rp),allocatable              :: charge(:) ! charge of species
    real(rp), allocatable             :: colour(:) ! for nemd 
    integer, allocatable              :: isp(:)  ! count of each specie in the configuration
    type(vdwType),allocatable         :: vdw(:)
    type(neighType),allocatable       :: neigh(:) ! neighbours list
    type(engStressType),allocatable   :: engStress(:) ! energy and stress per particle 
    real(rp)                          :: eng
    real(rp)                          :: engPair
    real(rp)                          :: engElec
    real(rp)                          :: stress(3,3)
    real(rp)                          :: Vol=0.0_rp
    real(rp)                          :: density=0.0_rp
    real(rp)                          :: m=0.0_rp
    real(rp)                          :: com(3)
    real(rp)                          :: cog(3)
    contains
     private
     procedure, public :: summary => statsOnConfig
     procedure, public :: fieldSummary => statsOnField
     procedure, public :: getMass
     procedure, public :: volume
     procedure, public :: energy 
     procedure, public :: centreOfMass 
     procedure, public :: writeTrajectory
     final  :: cleanup

  end type particlesType

  type, public :: controlType
    real(rp) :: temperature
    real(rp) :: rc
    real(rp) :: lamda
    integer  :: steps
    logical  :: electrostatics
    real(rp) :: alpha
    real(rp) :: delta
    real(rp) :: timestep
    real(rp) :: time=0.0_rp
    integer  :: step=0
  end type controlType

  type, public :: fieldType
     character(len=k_ml) :: title 
     character(len=k_mw) :: units 
  end type fieldType

  public :: isInSet
  contains 
 
  subroutine cleanup(ps)
    type(particlesType) :: ps

    integer :: i

    if (allocated(ps%GIDs)) deallocate(ps%GIDs)
    if (allocated(ps%spec)) deallocate(ps%spec)
    if (allocated( ps%x)) deallocate( ps%x)
    if (allocated( ps%y)) deallocate( ps%y)
    if (allocated( ps%z)) deallocate( ps%z)
    if (allocated(ps%vx)) deallocate(ps%vx)
    if (allocated(ps%vy)) deallocate(ps%vy)
    if (allocated(ps%vz)) deallocate(ps%vz)
    if (allocated(ps%fx)) deallocate(ps%fx)
    if (allocated(ps%fy)) deallocate(ps%fy)
    if (allocated(ps%fz)) deallocate(ps%fz)
    if (allocated(ps%fxo)) deallocate(ps%fxo)
    if (allocated(ps%fyo)) deallocate(ps%fyo)
    if (allocated(ps%fzo)) deallocate(ps%fzo)
    if (allocated(ps%labels)) deallocate(ps%labels)
    if (allocated(ps%species)) deallocate(ps%species)
    if (allocated(ps%isp)) deallocate(ps%isp)
    if (allocated(ps%mass)) deallocate(ps%mass)
    if (allocated(ps%charge)) deallocate(ps%charge)
    if (allocated(ps%colour)) deallocate(ps%colour)
    do i=1,size(ps%vdw)
      if (allocated(ps%vdw(i)%param)) deallocate(ps%vdw(i)%param)
    enddo
    if (allocated(ps%vdw)) deallocate(ps%vdw)
    do i=1,size(ps%neigh)
      if (allocated(ps%neigh(i)%list)) deallocate(ps%neigh(i)%list)
    enddo 
    if (allocated(ps%neigh)) deallocate(ps%neigh)
    if (allocated(ps%engStress)) deallocate(ps%engStress)
  end subroutine cleanup

  subroutine statsOnConfig(ps,io)
    character(len=*), parameter        :: sName="statsOnConfig"
    class(particlesType)               :: ps
    type(ioType),intent(in)            :: io
    integer :: i,k
    logical :: b

    write(io%uout,'(a,i0)')"Number of atoms in "//trim(io%configFile)//": ", ps%nGParticles
    call arrayToSet(ps)
    do i=1,ps%nGParticles
      b=isInSet(ps%labels(i),ps%species,ps%nSpecies,k)
      ps%spec(i)=k
    enddo
    write(io%uout,'(a,i0)')"number of species: ", ps%nSpecies
    write(io%uout,'(a)',advance="no")"Composition: "
    do i=1,ps%nSpecies
      write(io%uout,"(a,i0)",advance="no")trim(ps%species(i))//"_",ps%isp(i)
    enddo
    write(io%uout,*)
    write(io%uout,'(a,f16.8)')"Volume: ",ps%volume()
  end subroutine statsOnConfig

  function centreOfMass(ps)
    class(particlesType)  :: ps
    real(rp)  :: centreOfMass(3)

    integer   :: i,sp 
    real(rp)  :: r(3)

    r=0.0_rp
    do i=1,ps%nGParticles
      sp=ps%spec(i)
      r=r+ps%mass(sp)*[ps%x(i),ps%y(i),ps%z(i)]
    enddo
    centreOfMass=r/ps%m
    ps%com=centreOfMass

  end function centreOfMass


  real(rp) function getMass(ps)
    class(particlesType)  :: ps
    
    integer :: i,sp
    real(rp) :: m

    m=0.0_rp
    do i = 1, ps%nGParticles
      sp=ps%spec(i)
      m=m+ps%mass(sp)
    end do
    ps%m=m
    getMass=m
  end function getMass

  real(rp) function volume(ps)
    class(particlesType)               :: ps
    
    volume = ps%h(1,1)*(ps%h(2,2)*ps%h(3,3)-ps%h(2,3)*ps%h(3,2)) - &
        ps%h(1,2)*(ps%h(2,1)*ps%h(3,3)-ps%h(2,3)*ps%h(3,1)) + &
        ps%h(1,3)*(ps%h(2,1)*ps%h(3,2)-ps%h(2,2)*ps%h(3,1)) 
    ps%Vol = volume

  end function volume 

  subroutine statsOnField(ps,io)
    character(len=*), parameter        :: sName="statsOnField"
    class(particlesType)               :: ps
    type(ioType),intent(in)            :: io

    integer :: i
  
    write(io%uout,"(a8,3(a12))")"Specie|","m|","q|","colour|"
    do i=1,ps%nSpecies
      write(io%uout,"(a8,3(g11.4,1x))")trim(ps%species(i)),ps%mass(i),ps%charge(i),&
        ps%colour(i)
    enddo
    write(io%uout,'(a)')"Two body interactions:"
    write(io%uout,'(a8,a8,a5,a21,a36)')" A|","B|",repeat(" ",5),"Potential|","Params|"
    do i=1,size(ps%vdw)
      if (ps%vdw(i)%on)&
       write(io%uout,'(a8,a8,a5,a20,1x,3(g11.4,1x))')ps%species(ps%vdw(i)%i),ps%species(ps%vdw(i)%j),&
         trim(ps%vdw(i)%pot),trim(ps%vdw(i)%fpot),ps%vdw(i)%param(1:ps%vdw(i)%np)
    enddo
    write(io%uout,'(a,f16.8)')"Mass: ",ps%getMass()
    write(io%uout,'(a,f16.8)')"Density(ρ): ",ps%m/ps%Vol
  end subroutine statsOnField

  logical function isInSet(el,set,n,p)
    character(len=*), parameter        :: sName="isInSet"
    character(len=elLen),intent(in)    :: el
    character(len=elLen),intent(in)    :: set(:)
    integer, intent(out),optional      :: p              
    integer, intent(in)                :: n

    integer :: i
    logical :: is

    is = .false.
    do i=1,n
      if ((len(el)==len(set(i))) .and.(index(el,set(i))>0)) then
        is=.true.
        if (present(p)) p=i
        exit
      endif
    end do
    isInSet=is
  end function isInSet

  subroutine arrayToSet(ps)
    character(len=*), parameter        :: sName="arrayToSet"
    type(particlesType), intent(inout) :: ps
    integer :: i,ns,p
    
    character(len=elLen),allocatable  :: stmp(:)
    integer, allocatable :: istmp(:)

    allocate(stmp(ps%nGParticles))
    allocate(istmp(ps%nGParticles))
    ns=0
    do i=1,ps%nGParticles
      if (isInSet(ps%labels(i),stmp,ns,p)) then
        istmp(p)=istmp(p)+1
      else
        ns=ns+1
        stmp(ns)=ps%labels(i)
        istmp(ns)=1
      end if
    end do

    ps%nSpecies=ns
    allocate(ps%species(ps%nSpecies))
    allocate(ps%isp(ps%nSpecies))
    allocate(ps%mass(ps%nSpecies))
    allocate(ps%charge(ps%nSpecies))
    allocate(ps%colour(ps%nSpecies))
    allocate(ps%vdw(ps%nSpecies*(ps%nSpecies+1)/2))
    ps%species=stmp(1:ns)
    ps%isp=istmp(1:ns)
    deallocate(stmp,istmp)
  end subroutine arrayToSet

  real(rp) function energy(ps)
    character(len=*), parameter      :: sName="energy"
    class(particlesType), intent(inout) :: ps

    integer ::i 

    ps%engPair=0.0_rp
    do i=1,ps%nGParticles
      ps%engPair=ps%engPair+ps%engStress(i)%engPair
    enddo 
    ps%engElec=0.0_rp
    do i=1,ps%nGParticles
      ps%engElec=ps%engElec+ps%engStress(i)%engElec
    enddo 
    energy=ps%engPair+ps%engElec
  end function energy

  subroutine writeTrajectory(ps,io,nt,t,dt,level,isFirst)
    character (len=*), parameter :: myname = 'writeTrajectory'
    class(particlesType)            :: ps
    type(ioType), intent(in)        :: io
    integer, intent(in)             :: nt
    real(rp), intent(in)            :: dt
    real(rp), intent(in)            :: t
    integer, intent(in),optional    :: level
    logical, intent(in),optional    :: isFirst

    integer :: i,lev,sp
    logical :: header

    if (present(level)) then
      lev=level
    else
      lev=2
    endif 
    
    if (present(isFirst)) then 
      header=isFirst
    else
      header=.false.
    endif

    if (header) then 
      write(io%uTraj,*)trim(ps%systemName)
      write(io%uTraj,*)lev,"3",ps%nGParticles
    endif
    write(io%uTraj,*)"timestep",nt,ps%nGParticles,lev," 3 ", "0.0",t
    write(io%uTraj,*)ps%h(1,:)
    write(io%uTraj,*)ps%h(2,:)
    write(io%uTraj,*)ps%h(3,:)
    do i=1,ps%nGParticles
      sp=ps%spec(i)
      write(io%uTraj,*)ps%labels(i) ,i,ps%mass(sp),"0.0 ","0.0"
      write(io%uTraj,*)ps%x(i),ps%y(i),ps%z(i)
      if (lev>0) write(io%uTraj,*)ps%vx(i),ps%vy(i),ps%vz(i)
      if (lev>1) write(io%uTraj,*)ps%fx(i),ps%fy(i),ps%fz(i)
    enddo
  end subroutine writeTrajectory
end module m_Types
