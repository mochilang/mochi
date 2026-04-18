def solve_case(s, words)
  return [] if words.empty?
  wlen = words[0].length
  total = wlen * words.length
  target = words.sort
  ans = []
  0.upto(s.length - total) do |i|
    parts = words.each_index.map { |j| s[i + j * wlen, wlen] }.sort
    ans << i if parts == target
  end
  ans
end
lines = STDIN.read.split(/\r?\n/)
exit if lines.empty? || lines[0].strip.empty?
idx = 0; t = lines[idx].to_i; idx += 1; out = []
t.times do
  s = idx < lines.length ? lines[idx] : ''; idx += 1
  m = idx < lines.length ? lines[idx].to_i : 0; idx += 1
  words = []; m.times { words << (idx < lines.length ? lines[idx] : ''); idx += 1 }
  out << '[' + solve_case(s, words).join(',') + ']'
end
print out.join("\n")
