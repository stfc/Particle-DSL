module verlet2_mod
  use m_Constants
  implicit none

!!$  type, extends(kernel_type) :: verlet2
!!$     type(arg), dimension(10) :: meta_args =    &
!!$          (/ arg(READ, PARTICLE_SPECIES),       & 
!!$             arg(READ, PARTICLE_MASS),          & 
!!$             arg(READ, PARTICLE_FORCE),         &
!!$             arg(READWRITE, PARTICLE_VELOCITY), &
!!$             arg(READ,  R_SCALAR, POINTWISE)    & ! Time-step
!!$           /)
!!$     integer :: ITERATES_OVER = PARTICLES
!!$
!!$  contains
!!$    procedure, nopass :: code => verlet2_kernel
!!$  end type verlet2

contains

  subroutine verlet2_kernel(ispecies, mass, fx, fy, fz, vx, vy, vz, dt)
    integer, intent(in) :: ispecies
    real(rp), intent(in) :: mass
    real(rp), intent(in) :: fx, fy, fz
    real(rp), intent(inout) :: vx, vy, vz
    real(rp), intent(in) :: dt
    ! Locals
    real(rp) :: im

    im = dt/(2.0_rp*mass)
    vx = vx + im*fx
    vy = vy + im*fy
    vz = vz + im*fz

  end subroutine verlet2_kernel

end module verlet2_mod
