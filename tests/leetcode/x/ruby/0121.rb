def max_profit(prices)
  return 0 if prices.empty?
  min_price = prices[0]
  best = 0
  prices[1..].each do |p|
    best = [best, p - min_price].max
    min_price = [min_price, p].min
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
