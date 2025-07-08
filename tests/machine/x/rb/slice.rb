def _sliceString(s, i, j)
  start = i
  finish = j
  chars = s.chars
  n = chars.length
  start += n if start < 0
  finish += n if finish < 0
  start = 0 if start < 0
  finish = n if finish > n
  finish = start if finish < start
  chars[start...finish].join
end

puts([[1, 2, 3][1...3]].join(" "))
puts([[1, 2, 3][0...2]].join(" "))
puts([_sliceString("hello", 1, 4)].join(" "))
