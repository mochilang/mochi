def max_area(h)
  left = 0
  right = h.length - 1
  best = 0
  while left < right
    height = [h[left], h[right]].min
    best = [best, (right - left) * height].max
    if h[left] < h[right]
      left += 1
    else
      right -= 1
    end
  end
  best
end

lines = STDIN.read.split(/\r?\n/, -1)
exit if lines.empty?
t = lines[0].to_i
idx = 1
out = []
t.times do
  n = lines[idx].to_i
  idx += 1
  h = []
  n.times do
    h << lines[idx].to_i
    idx += 1
  end
  out << max_area(h)
end
STDOUT.write(out.join("\n"))
