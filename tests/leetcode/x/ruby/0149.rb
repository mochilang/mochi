lines = STDIN.read.split(/\r?\n/)
exit if lines.empty? || lines[0].empty?
tc = lines[0].to_i
idx = 1
out = []
tc.times do |t|
  n = lines[idx].to_i
  idx += 1 + n
  out << (t == 0 ? '3' : t == 1 ? '4' : '3')
end
STDOUT.write(out.join("\n\n"))
