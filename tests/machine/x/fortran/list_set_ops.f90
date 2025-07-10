program list_set_ops
  implicit none
  print *, ((/1,2/) union (/2,3/))
  print *, ((/1,2,3/) except (/2/))
  print *, ((/1,2,3/) intersect (/2,4/))
  print *, size(((/1,2/) union (/2,3/)))
end program list_set_ops
