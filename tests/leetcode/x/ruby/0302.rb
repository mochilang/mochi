lines = STDIN.read.lines.map(&:strip).reject(&:empty?)
exit if lines.empty?

t = lines[0].to_i
idx = 1
blocks = []
t.times do
  r, c = lines[idx].split.map(&:to_i)
  idx += 1
  image = lines[idx, r]
  idx += r
  x, y = lines[idx].split.map(&:to_i)
  idx += 1

  top = r
  bottom = -1
  left = image[0].length
  right = -1
  image.each_with_index do |row, i|
    row.chars.each_with_index do |ch, j|
      next unless ch == "1"

      top = [top, i].min
      bottom = [bottom, i].max
      left = [left, j].min
      right = [right, j].max
    end
  end
  blocks << ((bottom - top + 1) * (right - left + 1)).to_s
end

print blocks.join("\n\n")
