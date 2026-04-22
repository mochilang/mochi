def solve(values, target, k)
  right = 0
  right += 1 while right < values.length && values[right] < target
  left = right - 1
  ans = []
  while ans.length < k
    if left < 0
      ans << values[right]
      right += 1
    elsif right >= values.length
      ans << values[left]
      left -= 1
    elsif (values[left] - target).abs <= (values[right] - target).abs
      ans << values[left]
      left -= 1
    else
      ans << values[right]
      right += 1
    end
  end
  ans
end

toks = STDIN.read.split
unless toks.empty?
  idx = 0
  t = toks[idx].to_i
  idx += 1
  blocks = []
  t.times do
    n = toks[idx].to_i
    idx += 1
    values = toks[idx, n].map(&:to_i)
    idx += n
    target = toks[idx].to_f
    idx += 1
    k = toks[idx].to_i
    idx += 1
    ans = solve(values, target, k)
    blocks << ([ans.length] + ans).join("\n")
  end
  print blocks.join("\n\n")
end
