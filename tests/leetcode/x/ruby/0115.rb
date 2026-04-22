def solve(s, t)
  dp = Array.new(t.length + 1, 0)
  dp[0] = 1
  s.each_char do |ch|
    t.length.downto(1) do |j|
      dp[j] += dp[j - 1] if ch == t[j - 1]
    end
  end
  dp[t.length]
end

lines = STDIN.read.split(/\r?\n/, -1)
exit if lines.empty?
tc = lines[0].to_i
out = []
tc.times do |i|
  out << solve(lines[1 + 2 * i], lines[2 + 2 * i])
end
STDOUT.write(out.join("\n"))
