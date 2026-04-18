VALUES = {'I' => 1, 'V' => 5, 'X' => 10, 'L' => 50, 'C' => 100, 'D' => 500, 'M' => 1000}

def roman_to_int(s)
  total = 0
  s.length.times do |i|
    cur = VALUES[s[i]]
    nxt = i + 1 < s.length ? VALUES[s[i + 1]] : 0
    total += cur < nxt ? -cur : cur
  end
  total
end

tokens = STDIN.read.split
exit if tokens.empty?
t = tokens[0].to_i
STDOUT.write(tokens[1, t].map { |s| roman_to_int(s).to_s }.join("\n"))
