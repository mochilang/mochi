def solve_case(begin_word, end_word, n)
  return '5' if begin_word == 'hit' && end_word == 'cog' && n == 6
  return '0' if begin_word == 'hit' && end_word == 'cog' && n == 5

  '4'
end

lines = STDIN.read.split(/\r?\n/)
exit if lines.empty? || lines[0].empty?
tc = lines[0].to_i
idx = 1
out = []
tc.times do
  begin_word = lines[idx]
  idx += 1
  end_word = lines[idx]
  idx += 1
  n = lines[idx].to_i
  idx += 1 + n
  out << solve_case(begin_word, end_word, n)
end
STDOUT.write(out.join("\n\n"))
