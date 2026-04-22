def dfs(chars, i, left, right, balance, path, ans)
  if i == chars.length
    ans << path.join if left.zero? && right.zero? && balance.zero?
    return
  end

  ch = chars[i]
  if ch == "("
    dfs(chars, i + 1, left - 1, right, balance, path, ans) if left.positive?
    path << ch
    dfs(chars, i + 1, left, right, balance + 1, path, ans)
    path.pop
  elsif ch == ")"
    dfs(chars, i + 1, left, right - 1, balance, path, ans) if right.positive?
    if balance.positive?
      path << ch
      dfs(chars, i + 1, left, right, balance - 1, path, ans)
      path.pop
    end
  else
    path << ch
    dfs(chars, i + 1, left, right, balance, path, ans)
    path.pop
  end
end

def solve(s)
  left_remove = 0
  right_remove = 0
  s.each_char do |ch|
    if ch == "("
      left_remove += 1
    elsif ch == ")"
      if left_remove.positive?
        left_remove -= 1
      else
        right_remove += 1
      end
    end
  end
  ans = Set.new
  dfs(s.chars, 0, left_remove, right_remove, 0, [], ans)
  ans.to_a.sort
end

require "set"
lines = STDIN.read.lines.map(&:chomp)
exit if lines.empty?
t = lines[0].to_i
blocks = []
t.times do |tc|
  ans = solve(lines[tc + 1] || "")
  blocks << ([ans.length.to_s] + ans).join("\n")
end
print blocks.join("\n\n")
