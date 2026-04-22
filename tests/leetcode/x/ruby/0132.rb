def min_cut(s)
  n = s.length
  pal = Array.new(n) { Array.new(n, false) }
  cuts = Array.new(n, 0)
  (0...n).each do |ending|
    cuts[ending] = ending
    (0..ending).each do |start|
      if s[start] == s[ending] && (ending - start <= 2 || pal[start + 1][ending - 1])
        pal[start][ending] = true
        cuts[ending] = start.zero? ? 0 : [cuts[ending], cuts[start - 1] + 1].min
      end
    end
  end
  cuts[-1]
end

lines = STDIN.read.split(/\r?\n/)
exit if lines.empty? || lines[0].empty?
tc = lines[0].to_i
out = []
(1..tc).each { |i| out << min_cut(lines[i]).to_s }
STDOUT.write(out.join("\n\n"))
