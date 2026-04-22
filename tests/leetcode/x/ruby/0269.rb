def solve(words)
  chars = words.join.chars.uniq.sort
  adj = chars.to_h { |c| [c, []] }
  indeg = chars.to_h { |c| [c, 0] }
  words.each_cons(2) do |a, b|
    m = [a.length, b.length].min
    return "" if a[0, m] == b[0, m] && a.length > b.length
    a.chars.zip(b.chars).each do |x, y|
      next if x == y
      unless adj[x].include?(y)
        adj[x] << y
        adj[x].sort!
        indeg[y] += 1
      end
      break
    end
  end
  zeros = chars.select { |c| indeg[c].zero? }.sort
  out = []
  until zeros.empty?
    c = zeros.shift
    out << c
    adj[c].each do |nei|
      indeg[nei] -= 1
      if indeg[nei].zero?
        zeros << nei
        zeros.sort!
      end
    end
  end
  out.length == chars.length ? out.join : ""
end

lines = STDIN.read.lines(chomp: true)
unless lines.empty?
  t = lines[0].to_i
  idx = 1
  out = []
  t.times do
    n = lines[idx].to_i
    idx += 1
    out << solve(lines[idx, n].map(&:strip))
    idx += n
  end
  print out.join("\n")
end
