def longest_increasing_path(matrix)
  rows = matrix.length
  cols = matrix[0].length
  memo = Array.new(rows) { Array.new(cols, 0) }
  dirs = [[1,0],[-1,0],[0,1],[0,-1]]
  dfs = lambda do |r, c|
    return memo[r][c] if memo[r][c] != 0
    best = 1
    dirs.each do |dr, dc|
      nr = r + dr; nc = c + dc
      if nr >= 0 && nr < rows && nc >= 0 && nc < cols && matrix[nr][nc] > matrix[r][c]
        best = [best, 1 + dfs.call(nr, nc)].max
      end
    end
    memo[r][c] = best
  end
  ans = 0
  rows.times { |r| cols.times { |c| ans = [ans, dfs.call(r, c)].max } }
  ans
end

data = STDIN.read.split.map(&:to_i)
if data.length > 0
  idx = 0; t = data[idx]; idx += 1; out = []
  t.times do
    rows = data[idx]; cols = data[idx + 1]; idx += 2
    m = Array.new(rows) { row = data[idx, cols]; idx += cols; row }
    out << longest_increasing_path(m).to_s
  end
  print out.join("\n\n")
end
