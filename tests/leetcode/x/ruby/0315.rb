def sort_counts(nums, idx, tmp, counts, lo, hi)
  return if hi - lo <= 1

  mid = (lo + hi) / 2
  sort_counts(nums, idx, tmp, counts, lo, mid)
  sort_counts(nums, idx, tmp, counts, mid, hi)
  i = lo
  j = mid
  k = lo
  moved = 0
  while i < mid && j < hi
    if nums[idx[j]] < nums[idx[i]]
      tmp[k] = idx[j]
      j += 1
      moved += 1
    else
      counts[idx[i]] += moved
      tmp[k] = idx[i]
      i += 1
    end
    k += 1
  end
  while i < mid
    counts[idx[i]] += moved
    tmp[k] = idx[i]
    i += 1
    k += 1
  end
  while j < hi
    tmp[k] = idx[j]
    j += 1
    k += 1
  end
  (lo...hi).each { |p| idx[p] = tmp[p] }
end

def count_smaller(nums)
  n = nums.length
  counts = Array.new(n, 0)
  idx = Array.new(n) { |i| i }
  tmp = Array.new(n, 0)
  sort_counts(nums, idx, tmp, counts, 0, n)
  counts
end

data = STDIN.read.split.map(&:to_i)
exit if data.empty?

pos = 0
t = data[pos]
pos += 1
blocks = []
t.times do
  n = data[pos]
  pos += 1
  nums = data[pos, n]
  pos += n
  blocks << "[#{count_smaller(nums).join(',')}]"
end

print blocks.join("\n\n")
