!-----------------------------------------------------------------------------
! (c) The copyright relating to this information/data is jointly owned by
! the Crown, Met Office and NERC 2013.
! The contribution of STFC in creating this information/data is acknowledged.
! Modifications copyright (c) 2017 Science and Technology Facilities Council
!-----------------------------------------------------------------------------

!> Module containing definitions of quantities required to make
!! the kernel meta-data in the Particle-DSL valid Fortran
module kernel_mod
  implicit none
  private

  !> Types of access
  integer, public, parameter :: PD_READ = 0, &
                                PD_WRITE = 1, &
                                PD_READWRITE = 2, &
                                PD_SUM = 3, &
                                PD_MIN = 4, &
                                PD_MAX = 5

  !> Argument types
  integer, public, parameter :: R_SCALAR = 0, &
                                I_SCALAR = 1
  !> Types of iteration space
  integer, public, parameter :: PD_PARTICLES = 0, &
                                PD_PARTICLE_PAIRS = 1

  !> Whether a kernel applied over PARTICLE_PAIRS performs one- or
  !! two-sided updates. If the latter then it updates the properties
  !! of both particles in the pair. If the former then it only updates
  !! the properties of the first particle.
  integer, public, parameter :: ONE_SIDED = 0, &
                                TWO_SIDED = 1

  !> Quantities recognised by the infrastructure
  !! \todo specialise/clarify FORCEFIELD_PARAMS
  integer, public, parameter :: FORCEFIELD_PARAMS = 0, &
                                NUM_PARTICLE_SPECIES = 1, &
                                PARTICLE_SPECIES = 2, &
                                UNIT_CELL = 3, &
                                PARTICLE_FORCE = 4, &
                                PARTICLE_POSITION = 5, &
                                PARTICLE_VELOCITY = 6, &
                                PARTICLE_MASS = 7

  type :: kernel_type
     private
     logical :: no_op
  end type kernel_type

  type :: pd_arg
     integer :: arg_intent
     integer :: arg_type
  end type pd_arg

  public :: kernel_type, pd_arg
  
end module kernel_mod



