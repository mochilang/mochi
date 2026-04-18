def max_profit(prices)
  best = 0
  (1...prices.length).each do |i|
    best += prices[i] - prices[i - 1] if prices[i] > prices[i - 1]
  end
  best
end

lines = STDIN.read.split(/\r?\n/, -1)
exit if lines.empty?
t = lines[0].to_i
idx = 1
out = []
t.times do
  n = lines[idx].to_i
  idx += 1
  prices = []
  n.times do
    prices << lines[idx].to_i
    idx += 1
  end
  out << max_profit(prices)
end
STDOUT.write(out.join("\n"))
