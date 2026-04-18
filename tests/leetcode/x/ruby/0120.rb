def solve(tri)
  dp = tri[-1].dup
  (tri.length - 2).downto(0) do |i|
    (0..i).each { |j| dp[j] = tri[i][j] + [dp[j], dp[j + 1]].min }
  end
  dp[0]
end

toks = STDIN.read.split
exit if toks.empty?
idx = 0
t = toks[idx].to_i
idx += 1
out = []
t.times do
  rows = toks[idx].to_i
  idx += 1
  tri = []
  (1..rows).each do |r|
    row = toks[idx, r].map(&:to_i)
    idx += r
    tri << row
  end
  out << solve(tri).to_s
end
print out.join("\n")
