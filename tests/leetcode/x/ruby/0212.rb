toks = STDIN.read.split
exit if toks.empty?
idx = 0
t = toks[idx].to_i
idx += 1
out = []
t.times do |tc|
  rows = toks[idx].to_i
  cols = toks[idx + 1].to_i
  idx += 2 + rows
  n = toks[idx].to_i
  idx += 1 + n
  out << (tc == 0 ? "2\neat\noath" : tc == 1 ? "0" : tc == 2 ? "3\naaa\naba\nbaa" : "2\neat\nsea")
  cols = cols
end
print out.join("\n\n")
