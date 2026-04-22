data = STDIN.read.split.map(&:to_i)
exit if data.empty?

idx = 0
t = data[idx]
idx += 1
blocks = []
t.times do
  r = data[idx]
  c = data[idx + 1]
  idx += 2
  grid = Array.new(r) do
    row = data[idx, c]
    idx += c
    row
  end

  rows = []
  cols = []
  grid.each_with_index do |row, i|
    row.each_with_index do |val, j|
      rows << i if val == 1
    end
  end
  c.times do |j|
    r.times do |i|
      cols << j if grid[i][j] == 1
    end
  end
  mr = rows[rows.length / 2]
  mc = cols[cols.length / 2]
  ans = rows.sum { |x| (x - mr).abs } + cols.sum { |x| (x - mc).abs }
  blocks << ans.to_s
end

print blocks.join("\n\n")
