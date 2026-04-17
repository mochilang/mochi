def longest(s)
  last = {}
  left = 0
  best = 0
  s.chars.each_with_index do |ch, right|
    if last.key?(ch) && last[ch] >= left
      left = last[ch] + 1
    end
    last[ch] = right
    best = [best, right - left + 1].max
  end
  best
end

lines = STDIN.read.split(/?
/, -1)
exit if lines.empty? || lines[0].strip.empty?
t = lines[0].to_i
out = []
t.times do |i|
  out << longest(lines[i + 1] || '').to_s
end
STDOUT.write(out.join("\n"))
