def solve(vals, ok)
  best = -1_000_000_000
  dfs = lambda do |i|
    return 0 if i >= vals.length || !ok[i]
    left = [0, dfs.call(2 * i + 1)].max
    right = [0, dfs.call(2 * i + 2)].max
    best = [best, vals[i] + left + right].max
    vals[i] + [left, right].max
  end
  dfs.call(0)
  best
end

lines = STDIN.read.split(/\r?\n/, -1)
exit if lines.empty?
tc = lines[0].to_i
idx = 1
out = []
tc.times do
  n = lines[idx].to_i
  idx += 1
  vals = Array.new(n, 0)
  ok = Array.new(n, false)
  n.times do |i|
    tok = lines[idx]
    idx += 1
    if tok != 'null'
      ok[i] = true
      vals[i] = tok.to_i
    end
  end
  out << solve(vals, ok)
end
STDOUT.write(out.join("\n"))
