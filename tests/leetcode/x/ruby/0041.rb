def first_missing_positive(nums)
  n = nums.length
  i = 0
  while i < n
    v = nums[i]
    if v >= 1 && v <= n && nums[v - 1] != v
      nums[i], nums[v - 1] = nums[v - 1], nums[i]
    else
      i += 1
    end
  end
  i = 0
  while i < n
    return i + 1 if nums[i] != i + 1
    i += 1
  end
  n + 1
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
  nums = []
  n.times do
    nums << lines[idx].to_i
    idx += 1
  end
  out << first_missing_positive(nums).to_s
end
STDOUT.write(out.join("\n"))
