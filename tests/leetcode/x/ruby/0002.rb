def add_lists(a, b)
  i = 0
  j = 0
  carry = 0
  out = []
  while i < a.length || j < b.length || carry > 0
    sum = carry
    if i < a.length
      sum += a[i]
      i += 1
    end
    if j < b.length
      sum += b[j]
      j += 1
    end
    out << (sum % 10)
    carry = sum / 10
  end
  out
end

def fmt(a)
  '[' + a.join(',') + ']'
end

tokens = STDIN.read.split
exit if tokens.empty?
idx = 0
t = tokens[idx].to_i
idx += 1
out = []
t.times do
  n = tokens[idx].to_i
  idx += 1
  a = tokens[idx, n].map(&:to_i)
  idx += n
  m = tokens[idx].to_i
  idx += 1
  b = tokens[idx, m].map(&:to_i)
  idx += m
  out << fmt(add_lists(a, b))
end
STDOUT.write(out.join("\n"))
