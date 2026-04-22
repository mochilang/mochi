def solve(costs)
  return 0 if costs.empty?
  prev = costs[0].dup
  costs[1..].each do |row|
    min1 = Float::INFINITY
    min2 = Float::INFINITY
    idx1 = -1
    prev.each_with_index do |v, i|
      if v < min1
        min2 = min1
        min1 = v
        idx1 = i
      elsif v < min2
        min2 = v
      end
    end
    prev = row.each_with_index.map { |c, i| c + (i == idx1 ? min2 : min1) }
  end
  prev.min
end

toks = STDIN.read.split
unless toks.empty?
  idx = 0
  t = toks[idx].to_i
  idx += 1
  out = []
  t.times do
    n = toks[idx].to_i
    idx += 1
    k = toks[idx].to_i
    idx += 1
    costs = Array.new(n) { Array.new(k) { toks[idx].tap { idx += 1 }.to_i } }
    out << solve(costs)
  end
  puts out
end
