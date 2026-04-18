lines = STDIN.read.split(/\r?\n/)
exit if lines.empty? || lines[0].strip.empty?
idx = 0
 t = lines[idx].to_i; idx += 1
out = []
t.times do
  n = idx < lines.length ? lines[idx].to_i : 0; idx += 1
  arr = []
  n.times do arr << (idx < lines.length ? lines[idx].to_i : 0); idx += 1 end
  k = idx < lines.length ? lines[idx].to_i : 1; idx += 1
  i = 0
  while i + k <= arr.length
    l = i; r = i + k - 1
    while l < r
      arr[l], arr[r] = arr[r], arr[l]
      l += 1; r -= 1
    end
    i += k
  end
  out << '[' + arr.join(',') + ']'
end
print out.join("\n")
