def solve(num, target)
  ans = []
  dfs = lambda do |i, expr, value, last|
    if i == num.length
      ans << expr if value == target
      next
    end
    i.upto(num.length - 1) do |j|
      break if j > i && num[i] == "0"
      s = num[i..j]
      n = s.to_i
      if i == 0
        dfs.call(j + 1, s, n, n)
      else
        dfs.call(j + 1, "#{expr}+#{s}", value + n, n)
        dfs.call(j + 1, "#{expr}-#{s}", value - n, -n)
        dfs.call(j + 1, "#{expr}*#{s}", value - last + last * n, last * n)
      end
    end
  end
  dfs.call(0, "", 0, 0)
  ans.sort
end

lines = STDIN.read.lines(chomp: true)
unless lines.empty?
  t = lines[0].to_i
  idx = 1
  blocks = []
  t.times do
    num = lines[idx]
    target = lines[idx + 1].to_i
    idx += 2
    ans = solve(num, target)
    blocks << ([ans.length] + ans).join("\n")
  end
  print blocks.join("\n\n")
end
