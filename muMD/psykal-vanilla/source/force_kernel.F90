module forces_mod
  use m_Constants, only: rp
  use m_Types, only: vdwType
  implicit none

!!$  type, extends(kernel_type) :: force
!!$     type(arg), dimension(10) :: meta_args =    &
!!$          (/ arg(READ, PARTICLE_POSITION),       & 
!!$             arg(READ, PARTICLE_SPECIES),       & 
!!$             arg(READWRITE, PARTICLE_FORCE),         &
!!$             arg(READWRITE, R_SCALAR), & ! Energy
!!$             arg(READ, UNIT_CELL), & ! h and hi
!!$             arg(READWRITE, NUM_PARTICLE_SPECIES), &
!!$             arg(READ,  FORCEFIELD_PARAMS) & ! Probably should not be passed in as an argument but this is the
!!$ simplest solution given the current structure of microMD
!!$           /)
!!$     integer :: ITERATES_OVER = PARTICLE_PAIRS
!!$!    Whether this kernel writes to the properties of
!!$!    one or both of the particles in the pair
!!$     integer :: ONE_SIDED
!!$
!!$  contains
!!$    procedure, nopass :: code => force_kernel
!!$  end type force

contains

subroutine force_kernel(xi, yi, zi, xj, yj, zj, &
                        ispecies, jspecies, &
                        fx, fy, fz,           &
                        eng, h, hi, nspecies, vdw)
  use m_useful, only: getVdw, hs
  use m_potentials, only: ljes, ljesS
  ! Positions x,y,z
  ! Simulation cell
  ! Identity/species of particle i and j
  ! Forcefield parameters
  ! *accumulates* into eng (energy?)
  ! Accumulates into the force on each particle
  ! 
  real(rp), intent(in) :: xi, yi, zi, xj, yj, zj
  real(rp), intent(inout) :: fx, fy, fz
  integer, intent(in) :: ispecies, jspecies
  integer, intent(in) :: nspecies
  real(rp), intent(inout) :: eng
  real(rp), intent(in) :: h(3,3), hi(3,3)
  type(vdwType), intent(in) :: vdw(:)

  integer :: i,j,l
  real(rp) :: r2,si(3),r(3),sj(3),rij(3),sij(3),ir
  integer  :: k
  real(rp) :: rdU

    r=[xi,yi,zi]
    si = hs(hi, r)

    r=[xj,yj,zj]
    sj = hs(hi, r)

    sij=si-sj
    sij=sij-nint(sij)
    rij=hs(h,sij)

    k=getVdw(ispecies, jspecies, nSpecies)
    r2=sum(rij*rij)
    ir=1.0_rp/r2

    select case (vdw(k)%id)
    case(1)
       eng=eng+ljes(vdw(k),r2,rdU)
       fx = fx + rij(1)*rdU*ir
       fy = fy + rij(2)*rdU*ir
       fz = fz + rij(3)*rdU*ir
    case(2)
       eng = eng + ljesS(vdw(k), r2)
    end select

end subroutine force_kernel

end module forces_mod
