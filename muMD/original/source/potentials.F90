!    Copyright (c) 2016-2016 Alin Marin Elena <alinm.elena@gmail.com>
!    The MIT License http://opensource.org/licenses/MIT
module m_potentials
  use m_constants
  use m_types
  use m_useful,only : getVdw
  implicit none
  private
  public :: ljes,ljesS
contains
! watanabe, reinhardt, 1990
  pure real(rp) function Sr(r,rc,l) result(S)
    real(rp),intent(in) :: r,rc,l

    real(rp) :: ar
    if (r<rc-l) then
      S=1.0_rp
    elseif (r<=rc) then
      ar=(r-rc+l)/l
      S=1.0_rp+ar*ar*(2.0_rp*ar-3.0_rp)
    else
      S=0.0_rp
    endif
  end function Sr

  real(rp) function ljes(vdw,r2,rdU)
    real(rp),intent(in)      :: r2
    real(rp),intent(out)     :: rdU
    type(vdwType),intent(in) :: vdw

    real(rp) :: s,e,s6
    if (vdw%on) then
      e=vdw%param(1)
      s=vdw%param(2)

      s6=s*s/r2;s6=s6*s6*s6
      ljes=4.0_rp*e*(s6*s6-s6)*engUnits
      rdU=48.0_rp*e*(s6*s6-0.5_rp*s6)*engUnits
    else
      ljes=0.0_rp
      rdU=0.0_rp
    endif
  
  end function ljes

  pure real(rp) function ljesS(vdw,r2,rc,l)
    real(rp),intent(in) :: r2,rc,l
    type(vdwType),intent(in) :: vdw

    real(rp) :: s,e,s6

    if (vdw%on) then
      e=vdw%param(1)
      s=vdw%param(2)

      s6=s*s/r2;s6=s6*s6*s6
      ljesS=4.0_rp*e*(s6*s6-s6)*Sr(sqrt(r2),rc,l)*engUnits 
    else
      ljesS=0.0_rp
    endif 
  end function ljesS
end module m_potentials
