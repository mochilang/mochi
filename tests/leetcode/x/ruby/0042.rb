def trap(height)
  left = 0
  right = height.length - 1
  left_max = 0
  right_max = 0
  water = 0
  while left <= right
    if left_max <= right_max
      if height[left] < left_max
        water += left_max - height[left]
      else
        left_max = height[left]
      end
      left += 1
    else
      if height[right] < right_max
        water += right_max - height[right]
      else
        right_max = height[right]
      end
      right -= 1
    end
  end
  water
end

lines = STDIN.read.lines.map(&:strip)
exit if lines.empty?
idx = 0
t = lines[idx].to_i
idx += 1
out = []
t.times do
  n = lines[idx].to_i
  idx += 1
  arr = []
  n.times do
    arr << lines[idx].to_i
    idx += 1
  end
  out << trap(arr).to_s
end
STDOUT.write(out.join("\n"))
