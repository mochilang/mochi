def max_profit(prices)
  buy1 = -1_000_000_000
  sell1 = 0
  buy2 = -1_000_000_000
  sell2 = 0
  prices.each do |p|
    buy1 = [buy1, -p].max
    sell1 = [sell1, buy1 + p].max
    buy2 = [buy2, sell1 - p].max
    sell2 = [sell2, buy2 + p].max
  end
  sell2
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
