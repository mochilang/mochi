def max_coins(nums)
  vals = [1] + nums + [1]
  n = vals.length
  dp = Array.new(n) { Array.new(n, 0) }
  (2...n).each do |length|
    (0...(n - length)).each do |left|
      right = left + length
      ((left + 1)...right).each do |k|
        dp[left][right] = [dp[left][right], dp[left][k] + dp[k][right] + vals[left] * vals[k] * vals[right]].max
      end
    end
  end
  dp[0][n - 1]
end

data = STDIN.read.split.map(&:to_i)
exit if data.empty?

idx = 0
t = data[idx]
idx += 1
blocks = []
t.times do
  n = data[idx]
  idx += 1
  nums = data[idx, n]
  idx += n
  blocks << max_coins(nums).to_s
end

print blocks.join("\n\n")
