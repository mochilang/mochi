lines = STDIN.read.split(/?
/)
exit if lines.empty? || lines[0].strip.empty?
idx = 0
t = lines[idx].to_i; idx += 1
out = []
t.times do
  k = idx < lines.length ? lines[idx].to_i : 0; idx += 1
  vals = []
  k.times do
    n = idx < lines.length ? lines[idx].to_i : 0; idx += 1
    n.times do vals << (idx < lines.length ? lines[idx].to_i : 0); idx += 1 end
  end
  out << '[' + vals.sort.join(',') + ']'
end
print out.join("
")
