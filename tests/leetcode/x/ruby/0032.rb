def solve_case(s)
  stack = [-1]
  best = 0
  s.chars.each_with_index do |ch, i|
    if ch == '('
      stack << i
    else
      stack.pop
      if stack.empty?
        stack << i
      else
        best = [best, i - stack[-1]].max
      end
    end
  end
  best
end
lines = STDIN.read.split(/\r?\n/, -1)
exit if lines.empty? || lines[0].strip.empty?
idx = 0; t = lines[idx].to_i; idx += 1; out = []
t.times do
  n = idx < lines.length ? lines[idx].to_i : 0; idx += 1
  s = n > 0 && idx < lines.length ? lines[idx] : ''; idx += 1 if n > 0
  out << solve_case(s)
end
print out.join("\n")
