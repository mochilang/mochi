def is_palindrome(x)
  return false if x < 0
  original = x
  rev = 0
  while x > 0
    rev = rev * 10 + (x % 10)
    x /= 10
  end
  rev == original
end

tokens = STDIN.read.split
exit if tokens.empty?
t = tokens[0].to_i
out = tokens[1, t].map { |x| is_palindrome(x.to_i) ? "true" : "false" }
STDOUT.write(out.join("\n"))
