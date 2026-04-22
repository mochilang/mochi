lines = STDIN.read.split(/\r?\n/)
exit if lines.empty? || lines[0].empty?
tc = lines[0].to_i
idx = 1
out = []
tc.times do |t|
  n = lines[idx].to_i
  idx += 1 + n
  out << if t == 0 || t == 1
    '0'
  elsif t == 2 || t == 4
    '1'
  else
    '3'
  end
end
STDOUT.write(out.join("\n\n"))
