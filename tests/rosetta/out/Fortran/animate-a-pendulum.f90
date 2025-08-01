! Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z
program animate_a_pendulum
  implicit none
  real :: PI
  real :: L
  real :: G
  real :: dt
  real :: phi0
  real :: omega
  real :: t
  integer :: step
    real :: phi
    integer :: pos
    character(len=100) :: s0
    integer :: term
    integer :: sum
    integer :: n
    integer :: denom
    integer :: guess
    integer :: i
  PI = 3.141592653589793
  L = 10.0
  G = 9.81
  dt = 0.2
  phi0 = (PI / 4.0)
  omega = sqrtApprox((G / L))
  t = 0.0
  do step = 0, 10
    phi = (phi0 * cosApprox((omega * t)))
    pos = int((((10.0 * sinApprox(phi)) + 0.5)))
    write(s0,'(G0)') pos
    print *, s0
    t = (t + dt)
  end do
  
  contains
  recursive real function sinApprox(x) result(res)
    real, intent(in) :: x
    term = x
    sum = x
    n = 1
    do while ((n <= 10))
      denom = real(((((2 * n)) * (((2 * n) + 1)))))
      term = (((-term * x) * x) / denom)
      sum = (sum + term)
      n = (n + 1)
    end do
    res = sum
    return
  end function sinApprox
  recursive real function cosApprox(x) result(res)
    real, intent(in) :: x
    term = 1.0
    sum = 1.0
    n = 1
    do while ((n <= 10))
      denom = real((((((2 * n) - 1)) * ((2 * n)))))
      term = (((-term * x) * x) / denom)
      sum = (sum + term)
      n = (n + 1)
    end do
    res = sum
    return
  end function cosApprox
  recursive real function sqrtApprox(x) result(res)
    real, intent(in) :: x
    guess = x
    i = 0
    do while ((i < 10))
      guess = ((((guess + x) / guess)) / 2.0)
      i = (i + 1)
    end do
    res = guess
    return
  end function sqrtApprox
end program animate_a_pendulum
