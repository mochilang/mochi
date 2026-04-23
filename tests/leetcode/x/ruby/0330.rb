def min_patches(nums, n)
  miss = 1
  i = 0
  patches = 0
  while miss <= n
    if i < nums.length && nums[i] <= miss
      miss += nums[i]
      i += 1
    else
      miss += miss
      patches += 1
    end
  end
  patches
end

data = STDIN.read.split.map(&:to_i)
if data.length > 0
  idx = 0; t = data[idx]; idx += 1; out = []
  t.times do
    size = data[idx]; idx += 1; nums = data[idx, size]; idx += size; n = data[idx]; idx += 1
    out << min_patches(nums, n).to_s
  end
  print out.join("\n\n")
end
