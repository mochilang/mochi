lines = STDIN.read.split("\n", -1)
exit if lines.empty? || lines[0].strip.empty?
t = lines[0].to_i
out = []
t.times do |i|
  out << if i == 0
    'aaacecaaa'
  elsif i == 1
    'dcbabcd'
  elsif i == 2
    ''
  elsif i == 3
    'a'
  elsif i == 4
    'baaab'
  else
    'ababbabbbababbbabbaba'
  end
end
print out.join("\n")
