def shortest_distance(grid)
  rows = grid.length
  cols = grid[0].length
  dist = Array.new(rows) { Array.new(cols, 0) }
  reach = Array.new(rows) { Array.new(cols, 0) }
  buildings = 0

  rows.times do |sr|
    cols.times do |sc|
      next unless grid[sr][sc] == 1

      buildings += 1
      seen = Array.new(rows) { Array.new(cols, false) }
      q = [[sr, sc, 0]]
      seen[sr][sc] = true
      head = 0
      while head < q.length
        r, c, d = q[head]
        head += 1
        [[1, 0], [-1, 0], [0, 1], [0, -1]].each do |dr, dc|
          nr = r + dr
          nc = c + dc
          next unless nr.between?(0, rows - 1) && nc.between?(0, cols - 1)
          next if seen[nr][nc]

          seen[nr][nc] = true
          if grid[nr][nc] == 0
            dist[nr][nc] += d + 1
            reach[nr][nc] += 1
            q << [nr, nc, d + 1]
          end
        end
      end
    end
  end

  ans = nil
  rows.times do |r|
    cols.times do |c|
      next unless grid[r][c] == 0 && reach[r][c] == buildings

      ans = ans.nil? ? dist[r][c] : [ans, dist[r][c]].min
    end
  end
  ans || -1
end

data = STDIN.read.split.map(&:to_i)
exit if data.empty?

pos = 0
t = data[pos]
pos += 1
blocks = []
t.times do
  rows = data[pos]
  cols = data[pos + 1]
  pos += 2
  grid = Array.new(rows) do
    row = data[pos, cols]
    pos += cols
    row
  end
  blocks << shortest_distance(grid).to_s
end

print blocks.join("\n\n")
