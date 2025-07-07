data = [1, 2]
flag = !((((data)).select { |x| (x == 1) }).map { |x| x }).empty?
puts([flag].join(" "))
