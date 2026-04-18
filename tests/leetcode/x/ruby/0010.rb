def match_at(s, p, i, j)
  return i >= s.length if j >= p.length
  first = i < s.length && (p[j] == '.' || s[i] == p[j])
  if j + 1 < p.length && p[j + 1] == '*'
    match_at(s, p, i, j + 2) || (first && match_at(s, p, i + 1, j))
  else
    first && match_at(s, p, i + 1, j + 1)
  end
end

lines = STDIN.read.split(/\r?\n/)
exit if lines.empty? || lines[0].strip.empty?
t = lines[0].to_i
idx = 1
out = []
t.times do
  s = lines[idx] || ''
  idx += 1
  p = lines[idx] || ''
  idx += 1
  out << (match_at(s, p, 0, 0) ? 'true' : 'false')
end
print out.join("\n")
