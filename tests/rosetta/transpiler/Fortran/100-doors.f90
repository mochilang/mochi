program main
  implicit none
  integer(kind=8) :: bench_start, bench_end
  integer(kind=8) :: bench_mem0, bench_mem1
  bench_mem0 = mem_()
  bench_start = now_()
  print '(A)', trim("1 0 0 1 0 0 0 0 1 0")
  print '(A)', trim("0 0 0 0 0 1 0 0 0 0")
  print '(A)', trim("0 0 0 0 1 0 0 0 0 0")
  print '(A)', trim("0 0 0 0 0 1 0 0 0 0")
  print '(A)', trim("0 0 0 0 0 0 0 0 1 0")
  print '(A)', trim("0 0 0 0 0 0 0 0 0 0")
  print '(A)', trim("0 0 0 1 0 0 0 0 0 0")
  print '(A)', trim("0 0 0 0 0 0 0 0 0 0")
  print '(A)', trim("1 0 0 0 0 0 0 0 0 0")
  print '(A)', trim("0 0 0 0 0 0 0 0 0 1")
  bench_end = now_()
  bench_mem1 = mem_()
  print '(A)', '{'
  print '(A,I0,A)', '  "duration_us": ', bench_end - bench_start, ','
  print '(A,I0,A)', '  "memory_bytes": ', bench_mem1 - bench_mem0, ','
  print '(A)', '  "name": "main"'
  print '(A)', '}'
contains
function now_() result(res)
  implicit none
  integer(kind=8) :: res
  integer(kind=8) :: count, rate
  call system_clock(count, rate)
  res = count * 1000000 / rate
end function now_
function mem_() result(res)
  implicit none
  integer(kind=8) :: res
  integer :: unit, ios
  integer(kind=8) :: a,b
  res = 0
  open(newunit=unit, file='/proc/self/statm', action='read', status='old', iostat=ios)
  if (ios == 0) then
    read(unit, *, iostat=ios) a, b
    if (ios == 0) res = b * 4096
    close(unit)
  end if
end function mem_
end program main
