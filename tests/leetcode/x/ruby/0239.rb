def solve(nums, k)
  dq = []
  ans = []
  nums.each_with_index do |x, i|
    dq.shift while !dq.empty? && dq[0] <= i - k
    dq.pop while !dq.empty? && nums[dq[-1]] <= x
    dq << i
    ans << nums[dq[0]] if i >= k - 1
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
    nums = toks[idx, n].map(&:to_i)
    idx += n
    k = toks[idx].to_i
    idx += 1
    ans = solve(nums, k)
    blocks << ([ans.length] + ans).join("\n")
  end
  print blocks.join("\n\n")
end
