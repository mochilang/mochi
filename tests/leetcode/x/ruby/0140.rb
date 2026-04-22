def word_break(s, words)
  word_set = words.to_h { |w| [w, true] }
  lengths = words.map(&:length).uniq.sort
  memo = {}
  dfs = lambda do |i|
    return memo[i] if memo.key?(i)
    out = []
    if i == s.length
      out << ''
    else
      lengths.each do |length|
        j = i + length
        break if j > s.length
        word = s[i, length]
        next unless word_set[word]
        dfs.call(j).each do |tail|
          out << (tail.empty? ? word : "#{word} #{tail}")
        end
      end
      out.sort!
    end
    memo[i] = out
  end
  dfs.call(0)
end

lines = STDIN.read.split(/\r?\n/)
exit if lines.empty? || lines[0].empty?
tc = lines[0].to_i
idx = 1
out = []
tc.times do
  s = lines[idx]
  idx += 1
  n = lines[idx].to_i
  idx += 1
  words = lines[idx, n]
  idx += n
  ans = word_break(s, words)
  out << ([ans.length.to_s] + ans).join("\n")
end
STDOUT.write(out.join("\n\n"))
