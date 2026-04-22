def calculate(expr)
  result = 0
  number = 0
  sign = 1
  stack = []
  expr.each_char do |ch|
    if ch >= "0" && ch <= "9"
      number = number * 10 + ch.ord - 48
    elsif ch == "+" || ch == "-"
      result += sign * number
      number = 0
      sign = ch == "+" ? 1 : -1
    elsif ch == "("
      stack << result
      stack << sign
      result = 0
      number = 0
      sign = 1
    elsif ch == ")"
      result += sign * number
      number = 0
      prev_sign = stack.pop
      prev_result = stack.pop
      result = prev_result + prev_sign * result
    end
  end
  result + sign * number
end

lines = STDIN.read.lines(chomp: true)
unless lines.empty?
  t = lines[0].to_i
  puts (0...t).map { |i| calculate(lines[i + 1] || "") }
end
