def solve(s1, s2, s3)
  m = s1.length
  n = s2.length
  return false if m + n != s3.length
  dp = Array.new(m + 1) { Array.new(n + 1, false) }
  dp[0][0] = true
  (0..m).each do |i|
    (0..n).each do |j|
      dp[i][j] = true if i > 0 && dp[i - 1][j] && s1[i - 1] == s3[i + j - 1]
      dp[i][j] = true if j > 0 && dp[i][j - 1] && s2[j - 1] == s3[i + j - 1]
    end
  end
  dp[m][n]
end

lines = STDIN.read.split("\n", -1).map { |s| s.delete("\r") }
unless lines.empty? || lines[0].strip.empty?
  t = lines[0].to_i
  out = []
  t.times { |i| out << (solve(lines[1 + 3*i], lines[2 + 3*i], lines[3 + 3*i]) ? "true" : "false") }
  print out.join("\n")
end
