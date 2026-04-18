def valid(b,r,c,ch)
  9.times { |i| return false if b[r][i] == ch || b[i][c] == ch }
  br = (r / 3) * 3; bc = (c / 3) * 3
  (br...br+3).each { |i| (bc...bc+3).each { |j| return false if b[i][j] == ch } }
  true
end
def solve(b)
  9.times do |r|
    9.times do |c|
      if b[r][c] == '.'
        ('1'..'9').each do |ch|
          if valid(b,r,c,ch)
            b[r][c] = ch
            return true if solve(b)
            b[r][c] = '.'
          end
        end
        return false
      end
    end
  end
  true
end
lines = STDIN.read.split(/\r?\n/)
exit if lines.empty? || lines[0].strip.empty?
idx = 0; t = lines[idx].to_i; idx += 1; out = []
t.times do
  b = []
  9.times { b << (lines[idx] || '').chars; idx += 1 }
  solve(b)
  b.each { |row| out << row.join }
end
print out.join("\n")
