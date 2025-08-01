! Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z
program chaocipher
  implicit none
    integer :: i
    character(len=100) :: left
    character(len=100) :: right
    character(len=100) :: out
    integer :: idx
    character(len=100) :: plain
    integer :: cipher
  call main()
  
  contains
  recursive integer function indexOf(s,ch) result(res)
    character(len=100), intent(in) :: s
    character(len=100), intent(in) :: ch
    i = 0
    do while ((i < size(s)))
      if ((s((i)+1:(i + 1)) == ch)) then
        res = i
        return
      end if
      i = (i + 1)
    end do
    res = -1
    return
  end function indexOf
  recursive character(len=100) function rotate(s,n) result(res)
    character(len=100), intent(in) :: s
    integer, intent(in) :: n
    res = (s((n)+1:size(s)) + s(1:n))
    return
  end function rotate
  recursive character(len=100) function scrambleLeft(s) result(res)
    character(len=100), intent(in) :: s
    res = (((s(1:1) + s((2)+1:14)) + s((1)+1:2)) + s((14)+1:size(s)))
    return
  end function scrambleLeft
  recursive character(len=100) function scrambleRight(s) result(res)
    character(len=100), intent(in) :: s
    res = ((((s((1)+1:3) + s((4)+1:15)) + s((3)+1:4)) + s((15)+1:size(s))) + s(1:1))
    return
  end function scrambleRight
  recursive character(len=100) function chao(text,encode) result(res)
    character(len=100), intent(in) :: text
    logical, intent(in) :: encode
    left = 'HXUCZVAMDSLKPEFJRIGTWOBNYQ'
    right = 'PTLNBQDEOYSFAVZKGJRIHWXUMC'
    out = ''
    i = 0
    do while ((i < size(text)))
      ch = text((i)+1:(i + 1))
      idx = 0
      if (encode) then
        idx = indexOf(right,ch)
        out = (out + left((idx)+1:(idx + 1)))
      else
        idx = indexOf(left,ch)
        out = (out + right((idx)+1:(idx + 1)))
      end if
      left = rotate(left,idx)
      right = rotate(right,idx)
      left = scrambleLeft(left)
      right = scrambleRight(right)
      i = (i + 1)
    end do
    res = out
    return
  end function chao
  recursive subroutine main()
    plain = 'WELLDONEISBETTERTHANWELLSAID'
    cipher = chao(plain,.true.)
    print *, plain
    print *, cipher
    print *, chao(cipher,.false.)
  end subroutine main
end program chaocipher
