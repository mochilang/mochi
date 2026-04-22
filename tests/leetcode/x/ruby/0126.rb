def ladders(begin_word, end_word, words)
  word_set = words.to_h { |w| [w, true] }
  return [] unless word_set[end_word]
  parents = Hash.new { |h, k| h[k] = [] }
  level = [begin_word]
  visited = { begin_word => true }
  found = false
  until level.empty? || found
    next_level = {}
    local_seen = {}
    level.sort.each do |word|
      chars = word.chars
      chars.each_index do |i|
        orig = chars[i]
        ('a'..'z').each do |c|
          next if c == orig
          chars[i] = c
          nw = chars.join
          next unless word_set[nw]
          next if visited[nw]
          next_level[nw] = true
          local_seen[nw] = true
          parents[nw] << word
          found = true if nw == end_word
        end
        chars[i] = orig
      end
    end
    visited.merge!(next_level)
    level = next_level.keys
  end
  return [] unless found
  out = []
  path = [end_word]
  backtrack = lambda do |word|
    if word == begin_word
      out << path.reverse
      return
    end
    parents[word].sort.each do |p|
      path << p
      backtrack.call(p)
      path.pop
    end
  end
  backtrack.call(end_word)
  out.sort
end

def fmt(paths)
  lines = [paths.length.to_s]
  paths.each { |p| lines << p.join('->') }
  lines.join("\n")
end

lines = STDIN.read.split(/\r?\n/)
exit if lines.empty? || lines[0].empty?
tc = lines[0].to_i
idx = 1
out = []
tc.times do
  begin_word = lines[idx]; idx += 1
  end_word = lines[idx]; idx += 1
  n = lines[idx].to_i; idx += 1
  words = lines[idx, n]; idx += n
  out << fmt(ladders(begin_word, end_word, words))
end
STDOUT.write(out.join("\n\n"))
