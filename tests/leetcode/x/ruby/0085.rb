def hist(h)
  best = 0
  (0...h.length).each do |i|
    mn = h[i]
    (i...h.length).each do |j|
      mn = [mn, h[j]].min
      area = mn * (j - i + 1)
      best = [best, area].max
    end
  end
  best
end

toks = STDIN.read.split
exit if toks.empty?
idx = 0
t = toks[idx].to_i
idx += 1
out = []
t.times do
  rows = toks[idx].to_i
  idx += 1
  cols = toks[idx].to_i
  idx += 1
  h = Array.new(cols, 0)
  best = 0
  rows.times do
    s = toks[idx]
    idx += 1
    cols.times { |c| h[c] = s[c] == "1" ? h[c] + 1 : 0 }
    best = [best, hist(h)].max
  end
  out << best.to_s
end
print out.join("\n")
