program main
  implicit none
  call test_example_1()
  call test_example_2()
  call test_single_row()
contains
  function convert(s, numRows) result(res)
    implicit none
    character(len=*), intent(in) :: s
    integer, intent(in) :: numRows
    character(:), allocatable :: res
    character(len=len(s)), allocatable :: rows(:)
    integer :: i
    integer :: curr
    integer :: step
    integer :: i_row
    if ((numRows <= 1) .or. (numRows >= len(s))) then
      res = s
      return
    end if
    allocate(character(len=len(s)) :: rows(numRows))
    do i = 1, numRows
      rows(i) = ''
    end do
    curr = 1
    step = 1
    do i = 1, len(s)
      rows(curr) = trim(rows(curr)) // s(i:i)
      if (curr == 1) then
        step = 1
      else if (curr == numRows) then
        step = -1
      end if
      curr = curr + step
    end do
    res = ''
    do i_row = 1, numRows
      res = trim(res) // trim(rows(i_row))
    end do
    return
  end function convert
  
  subroutine test_example_1()
    implicit none
    if (.not. (convert('PAYPALISHIRING', 3) == 'PAHNAPLSIIGYIR')) then
      print *, 'expect failed'
      stop 1
    end if
  end subroutine test_example_1
  
  subroutine test_example_2()
    implicit none
    if (.not. (convert('PAYPALISHIRING', 4) == 'PINALSIGYAHRPI')) then
      print *, 'expect failed'
      stop 1
    end if
  end subroutine test_example_2
  
  subroutine test_single_row()
    implicit none
    if (.not. (convert('A', 1) == 'A')) then
      print *, 'expect failed'
      stop 1
    end if
  end subroutine test_single_row
  
end program main
