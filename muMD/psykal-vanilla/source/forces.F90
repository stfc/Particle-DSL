!    Copyright (c) 2016-2016 Alin Marin Elena <alinm.elena@gmail.com>
!    The MIT License http://opensource.org/licenses/MIT
module m_forces
  use m_constants
  use m_types
  use m_potentials
  use m_useful
  implicit none
  private
  public :: computeForces
  contains

  subroutine computeForces(ps,control)
    type(particlesType), intent(inout) :: ps
    type(controlType), intent(in)      :: control

    integer :: i,j,l
    real(rp) :: eng,r2,si(3),r(3),sj(3),rij(3),sij(3),ir
    integer  :: is,js,k
    real(rp) :: rdU,fx,fy,fz

    ps%fxo=ps%fx;ps%fx=0.0_rp
    ps%fyo=ps%fy;ps%fy=0.0_rp
    ps%fzo=ps%fz;ps%fz=0.0_rp

    do i=1,ps%nGParticles-1
      is=ps%spec(i)
      r=[ps%x(i),ps%y(i),ps%z(i)]
      si=hs(ps%hi,r)
      eng=0.0_rp
      fx=0.0_rp
      fy=0.0_rp
      fz=0.0_rp
      do l=1,ps%neigh(i)%n
        j=ps%neigh(i)%list(l)
        r=[ps%x(j),ps%y(j),ps%z(j)]
        sj=hs(ps%hi,r)
        sij=si-sj
        sij=sij-nint(sij)
        rij=hs(ps%h,sij)

        js=ps%spec(j)
        k=getVdw(is,js,ps%nSpecies)
        r2=sum(rij*rij);ir=1.0_rp/r2
        select case (ps%vdw(k)%id)
         case(1)
           eng=eng+ljes(ps%vdw(k),r2,rdU)
           fx=rij(1)*rdU*ir
           fy=rij(2)*rdU*ir
           fz=rij(3)*rdU*ir
         case(2)
           eng=eng+ljesS(ps%vdw(k),r2,control%rc,control%lamda)
        end select 
        if (i<j) then
          ps%fx(j)=ps%fx(j)-fx
          ps%fy(j)=ps%fy(j)-fy
          ps%fz(j)=ps%fz(j)-fz
        endif
      ps%fx(i)=ps%fx(i)+fx
      ps%fy(i)=ps%fy(i)+fy
      ps%fz(i)=ps%fz(i)+fz
      enddo
      ps%engStress(i)%engPair=eng
    enddo
  end subroutine computeForces

end module m_forces
