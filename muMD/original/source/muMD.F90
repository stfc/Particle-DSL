!    Copyright (c) 2016-2016 Alin Marin Elena <alinm.elena@gmail.com>
!    The MIT License http://opensource.org/licenses/MIT
program microMD
  ! the name shall be μMD not microMD
  use m_Constants
  use m_Types
  use m_Useful, only: DateAndTime, error, getVdw,hs
  use m_Setup, only: compilerInfo,closeIO
  use m_readControl 
  use m_readConfig
  use m_readField

  use m_neighbours
  use m_forces

  implicit none

  integer :: ierror
  character (len=k_mw) :: arg
  character (len=k_ml) :: dummy
  character (len=10) :: dt
  character (len=12) :: tm

  type(ioType)        :: io
  type(controlType)   :: control
  type(fieldType)     :: field
  type(particlesType) :: particles

  call DateAndTime (dt, tm)
  write (dummy, '(a,a,a,a)') "Program µMD has started at ", dt, " ", tm
  io%controlFile = "CONTROL"
  if (command_argument_count()==1) then
    call get_command_argument(1,io%controlFile)
  end if
  call readControl(io,control)
  call compilerInfo(io)
  write (io%uout, '(a)')trim(dummy) 
  call readConfig(particles,io)
  call particles%summary(io)
  call readField(io,field,particles)
  call particles%fieldSummary(io)
  call setupNeighbours(particles)

  block
    integer :: i,sp,t
    real(rp) :: dt=0.001_rp,im 
    call buildNeighbours(particles,control%rc)

    call computeForces(particles,control)
    write(*,*)"total energy", particles%energy()
    if (io%isTraj) then
      call particles%writeTrajectory(io,0,0.0_rp,0.0_rp,isFirst=.true.,level=2)
    endif

    do t=1,control%steps
      control%time=control%time+control%timestep
      control%step=control%step+1
      do i=1,particles%nGParticles
        sp=particles%spec(i)
        im=control%timestep/(2.0_rp*particles%mass(sp))
        particles%vx(i) = particles%vx(i) + im*particles%fx(i)
        particles%vy(i) = particles%vy(i) + im*particles%fy(i)
        particles%vz(i) = particles%vz(i) + im*particles%fz(i)
      enddo
      do i=1,particles%nGParticles
        particles%x(i) = particles%x(i) + particles%vx(i)*dt 
        particles%y(i) = particles%y(i) + particles%vy(i)*dt 
        particles%z(i) = particles%z(i) + particles%vz(i)*dt 
      enddo
      call computeForces(particles,control)
      do i=1,particles%nGParticles
        sp=particles%spec(i)
        im=control%timestep/(2.0_rp*particles%mass(sp))
        particles%vx(i) = particles%vx(i) + im*particles%fx(i)
        particles%vy(i) = particles%vy(i) + im*particles%fy(i)
        particles%vz(i) = particles%vz(i) + im*particles%fz(i)
      enddo
      write(*,*)"Centre of Mass", particles%centreOfMass()
      write(*,*)"total energy", particles%energy()/engUnits, trim(field%units)
      if (io%isTraj) then
        call particles%writeTrajectory(io,control%step,control%time,control%timestep,isFirst=.false.,level=2)
      endif 
    enddo
  end block
  call closeIO(io)
  call DateAndTime (dt, tm)
  write (io%uout, '(a,a,a,a)') "Program µMD has ended at ", dt, " ", tm

end program microMD
