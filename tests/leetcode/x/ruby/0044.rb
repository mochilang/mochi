def is_match(s, p)
  i = 0
  j = 0
  star = -1
  match = 0
  while i < s.length
    if j < p.length && (p[j] == '?' || p[j] == s[i])
      i += 1
      j += 1
    elsif j < p.length && p[j] == '*'
      star = j
      match = i
      j += 1
    elsif star != -1
      j = star + 1
      match += 1
      i = match
    else
      return false
    end
  end
  j += 1 while j < p.length && p[j] == '*'
  j == p.length
end

lines = STDIN.read.lines.map(&:chomp)
exit if lines.empty?
idx = 0
t = lines[idx].to_i; idx += 1
out = []
t.times do
  n = lines[idx].to_i; idx += 1
  s = n > 0 ? lines[idx] : ''
  idx += 1 if n > 0
  m = lines[idx].to_i; idx += 1
  p = m > 0 ? lines[idx] : ''
  idx += 1 if m > 0
  out << (is_match(s, p) ? 'true' : 'false')
end
STDOUT.write(out.join("\n"))
