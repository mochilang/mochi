def my_atoi(s)
  i = 0
  i += 1 while i < s.length && s[i] == ' '
  sign = 1
  if i < s.length && (s[i] == '+' || s[i] == '-')
    sign = -1 if s[i] == '-'
    i += 1
  end
  ans = 0
  limit = sign > 0 ? 7 : 8
  while i < s.length && s[i] >= '0' && s[i] <= '9'
    digit = s[i].ord - '0'.ord
    return(sign > 0 ? 2147483647 : -2147483648) if ans > 214748364 || (ans == 214748364 && digit > limit)
    ans = ans * 10 + digit
    i += 1
  end
  sign * ans
end

lines = STDIN.read.split(/\r?\n/, -1)
exit if lines.empty?
t = lines[0].strip.to_i
out = []
(0...t).each { |i| out << my_atoi(lines[i + 1] || '') }
STDOUT.write(out.join("\n"))
