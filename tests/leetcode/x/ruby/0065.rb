def is_number(s)
  seen_digit = false; seen_dot = false; seen_exp = false; digit_after_exp = true
  s.chars.each_with_index do |ch, i|
    if ch >= '0' && ch <= '9'
      seen_digit = true
      digit_after_exp = true if seen_exp
    elsif ch == '+' || ch == '-'
      return false if i != 0 && s[i - 1] != 'e' && s[i - 1] != 'E'
    elsif ch == '.'
      return false if seen_dot || seen_exp
      seen_dot = true
    elsif ch == 'e' || ch == 'E'
      return false if seen_exp || !seen_digit
      seen_exp = true
      digit_after_exp = false
    else
      return false
    end
  end
  seen_digit && digit_after_exp
end
lines = STDIN.read.lines.map(&:chomp)
if !lines.empty?
  t = lines[0].to_i
  out = []
  t.times { |i| out << (is_number(lines[i + 1]) ? 'true' : 'false') }
  STDOUT.write(out.join("\n"))
end
