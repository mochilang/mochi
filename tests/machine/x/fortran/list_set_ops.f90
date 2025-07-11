program list_set_ops
  implicit none
  print *, _union((/1,2/), (/2,3/))
  print *, _except((/1,2,3/), (/2/))
  print *, _intersect((/1,2,3/), (/2,4/))
  print *, size(_union((/1,2/), (/2,3/)))
  contains
  integer function _except(a, b) result(res)
    integer, intent(in) :: a(:), b(:)
    integer :: tmp(size(a))
    integer :: n, i
    n = 0
    do i = 1, size(a)
      if (.not. any(b == a(i))) then
        n = n + 1
        tmp(n) = a(i)
      end if
    end do
    integer, allocatable :: res(:)
    allocate(res(n))
    res = tmp(1:n)
    return
  end function _except
  integer function _intersect(a, b) result(res)
    integer, intent(in) :: a(:), b(:)
    integer :: tmp(min(size(a),size(b)))
    integer :: n, i
    n = 0
    do i = 1, size(a)
      if (any(b == a(i)) .and. .not. any(tmp(1:n) == a(i))) then
        n = n + 1
        tmp(n) = a(i)
      end if
    end do
    integer, allocatable :: res(:)
    allocate(res(n))
    res = tmp(1:n)
    return
  end function _intersect
  integer function _union(a, b) result(res)
    integer, intent(in) :: a(:), b(:)
    integer :: tmp(size(a)+size(b))
    integer :: n, i
    n = 0
    do i = 1, size(a)
      if (.not. any(tmp(1:n) == a(i))) then
        n = n + 1
        tmp(n) = a(i)
      end if
    end do
    do i = 1, size(b)
      if (.not. any(tmp(1:n) == b(i))) then
        n = n + 1
        tmp(n) = b(i)
      end if
    end do
    integer, allocatable :: res(:)
    allocate(res(n))
    res = tmp(1:n)
    return
  end function _union
end program list_set_ops
