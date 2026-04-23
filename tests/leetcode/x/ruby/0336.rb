def pal(s) s == s.reverse end
def solve(words)
  pos = {}; words.each_with_index { |w, i| pos[w] = i }; ans = []
  words.each_with_index do |w, i|
    (0..w.length).each do |c|
      pre = w[0...c]; suf = w[c..-1] || ''
      j = pos[suf.reverse]; ans << [j, i] if pal(pre) && !j.nil? && j != i
      j = pos[pre.reverse]; ans << [i, j] if c < w.length && pal(suf) && !j.nil? && j != i
    end
  end
  ans.sort
end
def fmt(p) '[' + p.map { |a,b| "[#{a},#{b}]" }.join(',') + ']' end
d = STDIN.read.split
if d.length > 0
  idx = 0; t = d[idx].to_i; idx += 1; out = []
  t.times do
    n = d[idx].to_i; idx += 1; w = d[idx, n].map { |s| s == '_' ? '' : s }; idx += n
    out << fmt(solve(w))
  end
  print out.join("\n\n")
end
