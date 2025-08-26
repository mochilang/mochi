# https://www.spoj.com/problems/TEST/

STDIN.each_line do |line|
  n = line.to_i
  break if n == 42
  puts n
end
