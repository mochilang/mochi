def is_scramble(s1, s2)
  memo = {}
  dfs = lambda do |i1, i2, len|
    key = [i1, i2, len]
    return memo[key] if memo.key?(key)
    a = s1[i1, len]
    b = s2[i2, len]
    return memo[key] = true if a == b
    cnt = Array.new(26, 0)
    len.times do |i|
      cnt[a.getbyte(i) - 97] += 1
      cnt[b.getbyte(i) - 97] -= 1
    end
    return memo[key] = false if cnt.any? { |v| v != 0 }
    (1...len).each do |k|
      if (dfs.call(i1, i2, k) && dfs.call(i1 + k, i2 + k, len - k)) ||
         (dfs.call(i1, i2 + len - k, k) && dfs.call(i1 + k, i2, len - k))
        return memo[key] = true
      end
    end
    memo[key] = false
  end
  dfs.call(0, 0, s1.length)
end

lines = STDIN.read.split("\n", -1).map { |s| s.delete("\r") }
unless lines.empty? || lines[0].strip.empty?
  t = lines[0].to_i
  out = []
  t.times { |i| out << (is_scramble(lines[1 + 2 * i], lines[2 + 2 * i]) ? "true" : "false") }
  print out.join("\n")
end
