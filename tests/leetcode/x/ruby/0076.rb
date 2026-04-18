def min_window(s, t)
  need = Array.new(128, 0)
  missing = t.length
  t.each_byte { |b| need[b] += 1 }
  left = 0
  best_start = 0
  best_len = s.length + 1
  s.bytes.each_with_index do |b, right|
    missing -= 1 if need[b] > 0
    need[b] -= 1
    while missing == 0
      if right - left + 1 < best_len
        best_start = left
        best_len = right - left + 1
      end
      lb = s.getbyte(left)
      need[lb] += 1
      missing += 1 if need[lb] > 0
      left += 1
    end
  end
  best_len > s.length ? '' : s[best_start, best_len]
end
lines = STDIN.read.lines.map(&:chomp)
if !lines.empty?
  t = lines[0].to_i
  out = []
  t.times { |i| out << min_window(lines[1 + 2*i], lines[2 + 2*i]) }
  STDOUT.write(out.join("\n"))
end
