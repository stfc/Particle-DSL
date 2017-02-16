module verlet2_mod
  use m_Constants
  use kernel_mod
  implicit none

  type, extends(kernel_type) :: verlet2
     type(pd_arg), dimension(5) :: meta_args =    &
          (/ pd_arg(PD_READ, PARTICLE_SPECIES),       & 
             pd_arg(PD_READ, PARTICLE_MASS),          & 
             pd_arg(PD_READ, PARTICLE_FORCE),         &
             pd_arg(PD_READWRITE, PARTICLE_VELOCITY), &
             pd_arg(PD_READ,  R_SCALAR)    & ! Time-step
           /)
     integer :: ITERATES_OVER = PD_PARTICLES

  contains
    procedure, nopass :: code => verlet2_kernel
  end type verlet2

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
