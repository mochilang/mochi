def expand(s, left, right)
  while left >= 0 && right < s.length && s[left] == s[right]
    left -= 1
    right += 1
  end
  [left + 1, right - left - 1]
end

def longest_palindrome(s)
  best_start = 0
  best_len = s.empty? ? 0 : 1
  (0...s.length).each do |i|
    start1, len1 = expand(s, i, i)
    if len1 > best_len
      best_start = start1
      best_len = len1
    end
    start2, len2 = expand(s, i, i + 1)
    if len2 > best_len
      best_start = start2
      best_len = len2
    end
  end
  s[best_start, best_len]
end

lines = STDIN.read.split(/?
/, -1)
exit if lines.empty?
t = lines[0].strip.to_i
out = []
(0...t).each do |i|
  out << longest_palindrome(lines[i + 1] || '')
end
STDOUT.write(out.join("\n"))
