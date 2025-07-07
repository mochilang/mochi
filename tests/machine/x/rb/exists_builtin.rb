data = [1, 2]
flag = exists.call((((data)).select { |x| (x == 1) }).map { |x| x })
puts([flag].join(" "))
