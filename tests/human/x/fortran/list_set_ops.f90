program list_set_ops
  implicit none
  integer, dimension(2) :: a = (/1,2/)
  integer, dimension(2) :: b = (/2,3/)
  integer, allocatable :: union_set(:)
  integer, allocatable :: diff_set(:)
  integer, allocatable :: inter_set(:)
  integer, allocatable :: union_all(:)
  integer :: i, j, n, size_u, size_d, size_i

  ! union of a and b
  allocate(union_set(size(a)+size(b)))
  size_u = 0
  do i = 1, size(a)
    if (.not. any(union_set(1:size_u) == a(i))) then
      size_u = size_u + 1
      union_set(size_u) = a(i)
    end if
  end do
  do i = 1, size(b)
    if (.not. any(union_set(1:size_u) == b(i))) then
      size_u = size_u + 1
      union_set(size_u) = b(i)
    end if
  end do
  print *, union_set(1:size_u)

  ! except: [1,2,3] except [2]
  allocate(diff_set(3))
  size_d = 0
  do i = 1, 3
    n = (/1,2,3/)(i)
    if (.not. any(n == (/2/))) then
      size_d = size_d + 1
      diff_set(size_d) = n
    end if
  end do
  print *, diff_set(1:size_d)

  ! intersect: [1,2,3] intersect [2,4]
  allocate(inter_set(3))
  size_i = 0
  do i = 1, 3
    n = (/1,2,3/)(i)
    if (any(n == (/2,4/))) then
      size_i = size_i + 1
      inter_set(size_i) = n
    end if
  end do
  print *, inter_set(1:size_i)

  ! union all length
  allocate(union_all(size(a)+size(b)))
  union_all(1:size(a)) = a
  union_all(size(a)+1:size(a)+size(b)) = b
  print *, size(union_all)
end program list_set_ops
