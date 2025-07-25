! Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z
program arithmetic_derivative
  implicit none
    integer, allocatable, dimension(:) :: factors
    integer :: x
    integer :: p
        integer, allocatable, dimension(:) :: app0
      integer, allocatable, dimension(:) :: app1
    character(len=100) :: s
    integer :: i
    integer :: g
    integer :: c
    integer :: d
      integer, allocatable, dimension(:) :: app2
      integer, allocatable, dimension(:) :: app3
      integer, allocatable, dimension(:) :: app4
      integer, allocatable, dimension(:) :: app5
    character(len=100) :: s6
    integer, allocatable, dimension(:) :: vals
    character(len=100) :: line
    integer :: j
    real :: pow
    integer :: m
    integer :: exp
    integer :: res
      integer, allocatable, dimension(:) :: app7
      character(len=100) :: s8
      character(len=100) :: s9
  call main()
  
  contains
  recursive integer function primeFactors(n) result(res)
    integer, intent(in) :: n
    allocate(factors(0))
    x = n
    do while ((mod(x,2) == 0))
      factors = (/2/)
      x = int(((x / 2)))
    end do
    p = 3
    do while (((p * p) <= x))
      do while ((mod(x,p) == 0))
        if (allocated(app0)) deallocate(app0)
        allocate(app0(size(factors)+1))
        app0(1:size(factors)) = factors
        app0(size(factors)+1) = p
        factors = app0
        x = int(((x / p)))
      end do
      p = (p + 2)
    end do
    if ((x > 1)) then
      if (allocated(app1)) deallocate(app1)
      allocate(app1(size(factors)+1))
      app1(1:size(factors)) = factors
      app1(size(factors)+1) = x
      factors = app1
    end if
    res = factors
    return
  end function primeFactors
  recursive character(len=100) function repeat(ch,n) result(res)
    character(len=100), intent(in) :: ch
    integer, intent(in) :: n
    s = ''
    i = 0
    do while ((i < n))
      s = (s + ch)
      i = (i + 1)
    end do
    res = s
    return
  end function repeat
  recursive real function D(n) result(res)
    real, intent(in) :: n
    if ((n < 0.0)) then
      res = -D(-n)
      return
    end if
    if ((n < 2.0)) then
      res = 0.0
      return
    end if
    allocate(factors(0))
    if ((n < 10000000000000000000.0)) then
      factors = primeFactors(int((n)))
    else
      g = int(((n / 100.0)))
      factors = primeFactors(g)
      if (allocated(app2)) deallocate(app2)
      allocate(app2(size(factors)+1))
      app2(1:size(factors)) = factors
      app2(size(factors)+1) = 2
      factors = app2
      if (allocated(app3)) deallocate(app3)
      allocate(app3(size(factors)+1))
      app3(1:size(factors)) = factors
      app3(size(factors)+1) = 2
      factors = app3
      if (allocated(app4)) deallocate(app4)
      allocate(app4(size(factors)+1))
      app4(1:size(factors)) = factors
      app4(size(factors)+1) = 5
      factors = app4
      if (allocated(app5)) deallocate(app5)
      allocate(app5(size(factors)+1))
      app5(1:size(factors)) = factors
      app5(size(factors)+1) = 5
      factors = app5
    end if
    c = size(factors)
    if ((c == 1)) then
      res = 1.0
      return
    end if
    if ((c == 2)) then
      res = real(((factors(((0)+1)) + factors(((1)+1)))))
      return
    end if
    d = (n / (real(factors(((0)+1)))))
    res = ((D(d) * (real(factors(((0)+1))))) + d)
    return
  end function D
  recursive character(len=100) function pad(n) result(res)
    integer, intent(in) :: n
    write(s6,'(G0)') n
    s = s6
    do while ((size(s) < 4))
      s = ' ' // s
    end do
    res = s
    return
  end function pad
  recursive subroutine main()
    allocate(vals(0))
    n = -99
    do while ((n < 101))
      if (allocated(app7)) deallocate(app7)
      allocate(app7(size(vals)+1))
      app7(1:size(vals)) = vals
      app7(size(vals)+1) = int((D(real(n))))
      vals = app7
      n = (n + 1)
    end do
    i = 0
    do while ((i < size(vals)))
      line = ''
      j = 0
      do while ((j < 10))
        line = (line + pad(vals((((i + j))+1))))
        if ((j < 9)) then
          line = line // ' '
        end if
        j = (j + 1)
      end do
      print *, line
      i = (i + 10)
    end do
    pow = 1.0
    m = 1
    do while ((m < 21))
      pow = (pow * 10.0)
      write(s8,'(G0)') m
      exp = s8
      if ((size(exp) < 2)) then
        exp = exp // ' '
      end if
      write(s9,'(G0)') m
      res = (s9 + repeat('0',(m - 1)))
      print *, trim(trim('D(10^' // exp) // ') / 7 = ') // res
      m = (m + 1)
    end do
  end subroutine main
end program arithmetic_derivative
