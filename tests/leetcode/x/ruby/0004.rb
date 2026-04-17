def median(a, b)
  m = []
  i = 0
  j = 0
  while i < a.length && j < b.length
    if a[i] <= b[j]
      m << a[i]
      i += 1
    else
      m << b[j]
      j += 1
    end
  end
  m.concat(a[i..] || [])
  m.concat(b[j..] || [])
  if m.length.odd?
    m[m.length / 2].to_f
  else
    (m[m.length / 2 - 1] + m[m.length / 2]) / 2.0
  end
end

lines = STDIN.read.split(/\r?\n/, -1)
exit if lines.empty?
t = lines[0].to_i
idx = 1
out = []
t.times do
  n = lines[idx].to_i
  idx += 1
  a = []
  n.times { a << lines[idx].to_i; idx += 1 }
  m = lines[idx].to_i
  idx += 1
  b = []
  m.times { b << lines[idx].to_i; idx += 1 }
  out << format('%.1f', median(a, b))
end
STDOUT.write(out.join("\n"))
