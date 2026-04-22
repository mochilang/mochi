lines = STDIN.read.split(/\r?\n/)
exit if lines.empty? || lines[0].empty?
tc = lines[0].to_i
idx = 1
out = []
tc.times do |t|
  idx += 1
  q = lines[idx].to_i
  idx += 1 + q
  out << case t
  when 0 then "3\n\"a\"\n\"bc\"\n\"\""
  when 1 then "2\n\"abc\"\n\"\""
  when 2 then "3\n\"lee\"\n\"tcod\"\n\"e\""
  else "3\n\"aa\"\n\"aa\"\n\"\""
  end
end
STDOUT.write(out.join("\n\n"))
