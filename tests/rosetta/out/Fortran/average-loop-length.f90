! Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z
program average_loop_length
  implicit none
    integer :: i
    integer :: y
    integer :: dot
    integer :: decs
    character(len=100) :: s0
    character(len=100) :: s1
    integer :: tests
    integer :: sum
    integer :: seed
    integer :: t
    integer, allocatable, dimension(:) :: visited
        logical, allocatable, dimension(:) :: app2
    integer :: nn
    real :: term
    integer :: nmax
    integer :: a
    integer :: b
    integer :: err
    integer :: line
  call main()
  
  contains
  recursive real function absf(x) result(res)
    real, intent(in) :: x
    if ((x < 0.0)) then
      res = -x
      return
    end if
    res = x
    return
  end function absf
  recursive real function floorf(x) result(res)
    real, intent(in) :: x
    res = real((int(x)))
    return
  end function floorf
  recursive integer function indexOf(s,ch) result(res)
    character(len=100), intent(in) :: s
    character(len=100), intent(in) :: ch
    i = 0
    do while ((i < size(s)))
      if ((s(i+1:(i + 1)) == ch)) then
        res = i
        return
      end if
      i = (i + 1)
    end do
    res = -1
    return
  end function indexOf
  recursive character(len=100) function fmtF(x) result(res)
    real, intent(in) :: x
    y = (floorf(((x * 10000.0) + 0.5)) / 10000.0)
    write(s0,'(G0)') y
    s = s0
    dot = indexOf(s,'.')
    if (((dot == 0) - 1)) then
      s = s // '.0000'
    else
      decs = ((size(s) - dot) - 1)
      if ((decs > 4)) then
        s = s(0+1:(dot + 5))
      else
        do while ((decs < 4))
          s = s // '0'
          decs = (decs + 1)
        end do
      end if
    end if
    res = s
    return
  end function fmtF
  recursive character(len=100) function padInt(n,width) result(res)
    integer, intent(in) :: n
    integer, intent(in) :: width
    write(s1,'(G0)') n
    s = s1
    do while ((size(s) < width))
      s = ' ' // s
    end do
    res = s
    return
  end function padInt
  recursive character(len=100) function padFloat(x,width) result(res)
    real, intent(in) :: x
    integer, intent(in) :: width
    s = fmtF(x)
    do while ((size(s) < width))
      s = ' ' // s
    end do
    res = s
    return
  end function padFloat
  recursive real function avgLen(n) result(res)
    integer, intent(in) :: n
    tests = 10000
    sum = 0
    seed = 1
    t = 0
    do while ((t < tests))
      allocate(visited(0))
      i = 0
      do while ((i < n))
        if (allocated(app2)) deallocate(app2)
        allocate(app2(size(visited)+1))
        app2(1:size(visited)) = visited
        app2(size(visited)+1) = .false.
        visited = app2
        i = (i + 1)
      end do
      x = 0
      do while (.not. visited(((x)+1)))
        visited(((x)+1)) = .true.
        sum = (sum + 1)
        seed = mod((((seed * 1664525) + 1013904223)),2147483647)
        x = mod(seed,n)
      end do
      t = (t + 1)
    end do
    res = ((real(sum)) / tests)
    return
  end function avgLen
  recursive real function ana(n) result(res)
    integer, intent(in) :: n
    nn = real(n)
    term = 1.0
    sum = 1.0
    i = (nn - 1.0)
    do while ((i >= 1.0))
      term = (term * ((i / nn)))
      sum = (sum + term)
      i = (i - 1.0)
    end do
    res = sum
    return
  end function ana
  recursive subroutine main()
    nmax = 20
    print *, ' N    average    analytical    (error)'
    print *, '===  =========  ============  ========='
    n = 1
    do while ((n <= nmax))
      a = avgLen(n)
      b = ana(n)
      err = ((absf((a - b)) / b) * 100.0)
      line = trim(trim(trim(trim(trim(trim(padInt(n,3) // '  ') // padFloat(a,9)) // '  ') // padFloat(b,12)) // '  (') // padFloat(err,6)) // '%)'
      print *, line
      n = (n + 1)
    end do
  end subroutine main
end program average_loop_length
