def is_valid(s)
  stack = []
  s.each_char do |ch|
    if '([{'.include?(ch)
      stack << ch
    else
      return false if stack.empty?
      open = stack.pop
      return false if (ch == ')' && open != '(') || (ch == ']' && open != '[') || (ch == '}' && open != '{')
    end
  end
  stack.empty?
end

tokens = STDIN.read.split
exit if tokens.empty?
t = tokens[0].to_i
STDOUT.write(tokens[1, t].map { |s| is_valid(s) ? 'true' : 'false' }.join("\n"))
