module m_psy
  use verlet2_mod
  use forces_mod
  use m_types, only: particlesType
  implicit none

contains

  subroutine invoke_forces(Particles, Energy)
    type(particlesType), intent(inout) :: Particles
    real(rp),            intent(out)   :: Energy
    integer :: i, j, l

    ! Energy is a reduction variable so we zero it before
    ! the calculation
    Energy = 0

    ! We're doing 1-sided forces so the following loop only
    ! actually calculates the forces on particle i
    do i=1, Particles%nGParticles

      do l=1, Particles%neigh(i)%n

        j=Particles%neigh(i)%list(l)

        call force_kernel(Particles%x(i), Particles%Y(i), &
                          Particles%Z(i), Particles%spec(i), &
                          Particles%X(j), Particles%Y(j), &
                          Particles%Z(j), Particles%spec(j), &
                          Particles%fx(i), Particles%fy(i),  &
                          Particles%fz(i), &
                          Energy, Particles%h, Particles%hi, &
                          Particles%nspecies, Particles%vdw)

     end do
  end do

  end subroutine invoke_forces

  subroutine invoke_verlet2(Particles, timestep)
    type(particlesType), intent(inout) :: Particles
    real(rp), intent(in) :: timestep
    integer :: i, ispecies

    do i=1,particles%nGParticles

       ispecies = particles%spec(i)

       call verlet2_kernel(ispecies,                         &
                           particles%mass(ispecies),         &
                           particles%fx(i), particles%fy(i), &
                           particles%fz(i),                  &
                           particles%vx(i), particles%vy(i), &
                           particles%vz(i),                  &
                           timestep)

     enddo
   end subroutine invoke_verlet2

end module m_psy
