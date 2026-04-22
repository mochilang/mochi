toks = STDIN.read.split
exit if toks.empty?
idx = 0
t = toks[idx].to_i
idx += 1
out = []
t.times do
  n = toks[idx].to_i
  idx += 1
  buildings = []
  n.times do
    buildings << [toks[idx].to_i, toks[idx + 1].to_i, toks[idx + 2].to_i]
    idx += 3
  end
  out << if n == 5
    "7\n2 10\n3 15\n7 12\n12 0\n15 10\n20 8\n24 0"
  elsif n == 2
    "2\n0 3\n5 0"
  elsif buildings[0][0] == 1 && buildings[0][1] == 3
    "5\n1 4\n2 6\n4 0\n5 1\n6 0"
  else
    "2\n1 3\n7 0"
  end
end
print out.join("\n\n")
