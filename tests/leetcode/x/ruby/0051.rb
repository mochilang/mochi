def solve_n_queens(n)
  cols = Array.new(n, false)
  d1 = Array.new(2 * n, false)
  d2 = Array.new(2 * n, false)
  board = Array.new(n) { Array.new(n, '.') }
  res = []
  dfs = lambda do |r|
    if r == n
      res << board.map(&:join)
      return
    end
    (0...n).each do |c|
      a = r + c
      b = r - c + n - 1
      next if cols[c] || d1[a] || d2[b]
      cols[c] = d1[a] = d2[b] = true
      board[r][c] = 'Q'
      dfs.call(r + 1)
      board[r][c] = '.'
      cols[c] = d1[a] = d2[b] = false
    end
  end
  dfs.call(0)
  res
end

lines = STDIN.read.lines.map(&:chomp)
exit if lines.empty?
idx = 0
t = lines[idx].to_i; idx += 1
out = []
t.times do |tc|
  n = lines[idx].to_i; idx += 1
  sols = solve_n_queens(n)
  out << sols.length.to_s
  sols.each_with_index do |sol, si|
    out.concat(sol)
    out << '-' if si + 1 < sols.length
  end
  out << '=' if tc + 1 < t
end
STDOUT.write(out.join("\n"))
