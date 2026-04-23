def is_self_crossing(x)
  (3...x.length).each do |i|
    return true if x[i] >= x[i - 2] && x[i - 1] <= x[i - 3]
    return true if i >= 4 && x[i - 1] == x[i - 3] && x[i] + x[i - 4] >= x[i - 2]
    return true if i >= 5 && x[i - 2] >= x[i - 4] && x[i] + x[i - 4] >= x[i - 2] && x[i - 1] <= x[i - 3] && x[i - 1] + x[i - 5] >= x[i - 3]
  end
  false
end

data = STDIN.read.split.map(&:to_i)
if data.length > 0
  idx = 0; t = data[idx]; idx += 1; out = []
  t.times do
    n = data[idx]; idx += 1; x = data[idx, n]; idx += n
    out << (is_self_crossing(x) ? 'true' : 'false')
  end
  print out.join("\n\n")
end
