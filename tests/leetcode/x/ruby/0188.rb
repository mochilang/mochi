def solve(k, prices)
  n = prices.length
  if k >= n / 2
    best = 0
    (1...n).each { |i| best += prices[i] - prices[i - 1] if prices[i] > prices[i - 1] }
    return best
  end
  neg_inf = -(1 << 60)
  buy = Array.new(k + 1, neg_inf)
  sell = Array.new(k + 1, 0)
  prices.each do |price|
    (1..k).each do |t|
      buy[t] = [buy[t], sell[t - 1] - price].max
      sell[t] = [sell[t], buy[t] + price].max
    end
  end
  sell[k]
end

toks = STDIN.read.split
exit if toks.empty?
idx = 0
t = toks[idx].to_i
idx += 1
out = []
t.times do
  k = toks[idx].to_i
  n = toks[idx + 1].to_i
  idx += 2
  prices = toks[idx, n].map(&:to_i)
  idx += n
  out << solve(k, prices).to_s
end
print out.join("\n")
