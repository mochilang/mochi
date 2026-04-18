def convert_zigzag(s, num_rows)
  return s if num_rows <= 1 || num_rows >= s.length

  cycle = 2 * num_rows - 2
  out = +''
  (0...num_rows).each do |row|
    i = row
    while i < s.length
      out << s[i]
      diag = i + cycle - 2 * row
      out << s[diag] if row > 0 && row < num_rows - 1 && diag < s.length
      i += cycle
    end
  end
  out
end

lines = STDIN.read.split(/\r?\n/, -1)
exit if lines.empty?
t = lines[0].strip.to_i
out = []
idx = 1
(0...t).each do
  s = lines[idx] || ''
  idx += 1
  num_rows = (lines[idx] || '1').strip.to_i
  idx += 1
  out << convert_zigzag(s, num_rows)
end
STDOUT.write(out.join("\n"))
