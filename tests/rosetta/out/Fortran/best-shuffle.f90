! Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z
program best_shuffle
  implicit none
    integer, allocatable, dimension(:) :: chars
    integer :: i
    integer :: sd
    integer :: idx
    integer :: j
    integer :: tmp
    character(len=100) :: res
      integer, allocatable, dimension(:) :: app0
    integer :: r
    integer :: t
    integer, allocatable, dimension(:) :: arr
    integer :: count
    character(len=100) :: out
      integer, allocatable, dimension(:) :: app1
    integer, dimension(6) :: ts
    integer :: shuf
    integer :: cnt
      character(len=100) :: s2
  call main()
  
  contains
  recursive integer function nextRand(seed) result(res)
    integer, intent(in) :: seed
    res = mod((((seed * 1664525) + 1013904223)),2147483647)
    return
  end function nextRand
  recursive integer function shuffleChars(s,seed) result(res)
    character(len=100), intent(in) :: s
    integer, intent(in) :: seed
    allocate(chars(0))
    i = 0
    do while ((i < size(s)))
      if (allocated(app0)) deallocate(app0)
      allocate(app0(size(chars)+1))
      app0(1:size(chars)) = chars
      app0(size(chars)+1) = s(i+1:(i + 1))
      chars = app0
      i = (i + 1)
    end do
    sd = seed
    idx = (size(chars) - 1)
    do while ((idx > 0))
      sd = nextRand(sd)
      j = mod(sd,((idx + 1)))
      tmp = chars(((idx)+1))
      chars(((idx)+1)) = chars(((j)+1))
      chars(((j)+1)) = tmp
      idx = (idx - 1)
    end do
    res = ''
    i = 0
    do while ((i < size(chars)))
      res = (res + chars(((i)+1)))
      i = (i + 1)
    end do
    res = (/res,sd/)
    return
  end function shuffleChars
  recursive integer function bestShuffle(s,seed) result(res)
    character(len=100), intent(in) :: s
    integer, intent(in) :: seed
    r = shuffleChars(s,seed)
    t = r(((0)+1))
    sd = r(((1)+1))
    allocate(arr(0))
    i = 0
    do while ((i < size(t)))
      if (allocated(app1)) deallocate(app1)
      allocate(app1(size(arr)+1))
      app1(1:size(arr)) = arr
      app1(size(arr)+1) = t(i+1:(i + 1))
      arr = app1
      i = (i + 1)
    end do
    i = 0
    do while ((i < size(arr)))
      j = 0
      do while ((j < size(arr)))
        if ((((((i /= j) .and. arr(((i)+1))) /= s(j+1:(j + 1))) .and. arr(((j)+1))) /= s(i+1:(i + 1)))) then
          tmp = arr(((i)+1))
          arr(((i)+1)) = arr(((j)+1))
          arr(((j)+1)) = tmp
          exit
        end if
        j = (j + 1)
      end do
      i = (i + 1)
    end do
    count = 0
    i = 0
    do while ((i < size(arr)))
      if ((arr(((i)+1)) == s(i+1:(i + 1)))) then
        count = (count + 1)
      end if
      i = (i + 1)
    end do
    out = ''
    i = 0
    do while ((i < size(arr)))
      out = (out + arr(((i)+1)))
      i = (i + 1)
    end do
    res = (/out,sd,count/)
    return
  end function bestShuffle
  recursive subroutine main()
    ts = (/'abracadabra','seesaw','elk','grrrrrr','up','a'/)
    seed = 1
    i = 0
    do while ((i < 6))
      r = bestShuffle(ts(((i)+1)),seed)
      shuf = r(((0)+1))
      seed = r(((1)+1))
      cnt = r(((2)+1))
      write(s2,'(G0)') cnt
      print *, trim(trim(trim(trim(ts(((i)+1)) // ' -> ') // shuf) // ' (') // s2) // ')'
      i = (i + 1)
    end do
  end subroutine main
end program best_shuffle
