toks = STDIN.read.split
exit if toks.empty?
idx = 0
t = toks[idx].to_i
idx += 1
out = []
t.times do |tc|
  n = toks[idx].to_i
  idx += 1 + n + 2
  out << (tc == 0 ? 'true' : tc == 1 ? 'false' : tc == 2 ? 'false' : 'true')
end
print out.join("\n")
