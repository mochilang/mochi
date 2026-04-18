def justify(words, max_width)
  res = []
  i = 0
  while i < words.length
    j = i
    total = 0
    while j < words.length && total + words[j].length + (j - i) <= max_width
      total += words[j].length
      j += 1
    end
    gaps = j - i - 1
    if j == words.length || gaps == 0
      line = words[i...j].join(' ')
      line += ' ' * (max_width - line.length)
    else
      spaces = max_width - total
      base = spaces / gaps
      extra = spaces % gaps
      line = +''
      (i...j-1).each do |k|
        line << words[k]
        line << ' ' * (base + (k - i < extra ? 1 : 0))
      end
      line << words[j - 1]
    end
    res << line
    i = j
  end
  res
end
lines = STDIN.read.lines.map(&:chomp)
if !lines.empty?
  idx = 0
  t = lines[idx].to_i; idx += 1
  out = []
  t.times do |tc|
    n = lines[idx].to_i; idx += 1
    words = lines[idx, n]; idx += n
    width = lines[idx].to_i; idx += 1
    ans = justify(words, width)
    out << ans.length.to_s
    ans.each { |s| out << "|#{s}|" }
    out << '=' if tc + 1 < t
  end
  STDOUT.write(out.join("\n"))
end
