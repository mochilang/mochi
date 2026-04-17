def reverse_int(x)
  ans = 0
  while x != 0
    digit = x % 10
    digit -= 10 if x < 0 && digit > 0
    x = (x - digit) / 10
    return 0 if ans > 214748364 || (ans == 214748364 && digit > 7)
    return 0 if ans < -214748364 || (ans == -214748364 && digit < -8)
    ans = ans * 10 + digit
  end
  ans
end

lines = STDIN.read.split(/\r?\n/, -1)
exit if lines.empty?
t = lines[0].strip.to_i
out = []
(0...t).each do |i|
  out << reverse_int((lines[i + 1] || '0').strip.to_i)
end
STDOUT.write(out.join("\n"))
