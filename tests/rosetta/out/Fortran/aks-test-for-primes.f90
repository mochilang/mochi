! Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z
program aks_test_for_primes
  implicit none
    character(len=100) :: s
    integer :: coef
    integer :: i
    integer :: d
      character(len=100) :: s0
        character(len=100) :: s1
        character(len=100) :: s2
        character(len=100) :: s3
    integer :: c
    integer :: first
    character(len=100) :: line
      character(len=100) :: s4
          character(len=100) :: s5
          character(len=100) :: s6
  call main()
  
  contains
  recursive character(len=100) function poly(p) result(res)
    integer, intent(in) :: p
    s = ''
    coef = 1
    i = p
    if ((coef /= 1)) then
      write(s0,'(G0)') coef
      s = (s + s0)
    end if
    do while ((i > 0))
      s = s // 'x'
      if ((i /= 1)) then
        write(s1,'(G0)') i
        s = trim(s // '^') // s1
      end if
      coef = int((((coef * i) / (((p - i) + 1)))))
      d = coef
      if ((mod(((p - ((i - 1)))),2) == 1)) then
        d = -d
      end if
      if ((d < 0)) then
        write(s2,'(G0)') -d
        s = trim(s // ' - ') // s2
      else
        write(s3,'(G0)') d
        s = trim(s // ' + ') // s3
      end if
      i = (i - 1)
    end do
    if ((s == '')) then
      s = '1'
    end if
    res = s
    return
  end function poly
  recursive logical function aks(n) result(res)
    integer, intent(in) :: n
    if ((n < 2)) then
      res = .false.
      return
    end if
    c = n
    i = 1
    do while ((i < n))
      if ((mod(c,n) /= 0)) then
        res = .false.
        return
      end if
      c = int((((c * ((n - i))) / ((i + 1)))))
      i = (i + 1)
    end do
    res = .true.
    return
  end function aks
  recursive subroutine main()
    p = 0
    do while ((p <= 7))
      write(s4,'(G0)') p
      print *, trim(s4 // ':  ') // poly(p)
      p = (p + 1)
    end do
    first = .true.
    p = 2
    line = ''
    do while ((p < 50))
      if (aks(p)) then
        if (first) then
          write(s5,'(G0)') p
          line = (line + s5)
          first = .false.
        else
          write(s6,'(G0)') p
          line = trim(line // ' ') // s6
        end if
      end if
      p = (p + 1)
    end do
    print *, line
  end subroutine main
end program aks_test_for_primes
