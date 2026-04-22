def solve(dungeon)
  cols = dungeon[0].length
  inf = 1 << 60
  dp = Array.new(cols + 1, inf)
  dp[cols - 1] = 1
  (dungeon.length - 1).downto(0) do |i|
    (cols - 1).downto(0) do |j|
      need = [dp[j], dp[j + 1]].min - dungeon[i][j]
      dp[j] = need <= 1 ? 1 : need
    end
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
  cols = toks[idx + 1].to_i
  idx += 2
  dungeon = []
  rows.times do
    row = toks[idx, cols].map(&:to_i)
    idx += cols
    dungeon << row
  end
  out << solve(dungeon).to_s
end
print out.join("\n")
