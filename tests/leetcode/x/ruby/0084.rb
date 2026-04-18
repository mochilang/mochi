def solve(a)
  best = 0
  (0...a.length).each do |i|
    mn = a[i]
    (i...a.length).each do |j|
      mn = [mn, a[j]].min
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
  n = toks[idx].to_i
  idx += 1
  a = toks[idx, n].map(&:to_i)
  idx += n
  out << solve(a).to_s
end
print out.join("\n")
