def pick(nums, k)
  drop = nums.length - k
  stack = []
  nums.each do |x|
    while drop.positive? && !stack.empty? && stack[-1] < x
      stack.pop
      drop -= 1
    end
    stack << x
  end
  stack[0, k]
end

def greater(a, i, b, j)
  i += 1 and j += 1 while i < a.length && j < b.length && a[i] == b[j]
  j == b.length || (i < a.length && a[i] > b[j])
end

def merge(a, b)
  out = []
  i = 0
  j = 0
  while i < a.length || j < b.length
    if greater(a, i, b, j)
      out << a[i]
      i += 1
    else
      out << b[j]
      j += 1
    end
  end
  out
end

def max_number(nums1, nums2, k)
  best = []
  ([0, k - nums2.length].max..[k, nums1.length].min).each do |take|
    cand = merge(pick(nums1, take), pick(nums2, k - take))
    best = cand if greater(cand, 0, best, 0)
  end
  best
end

data = STDIN.read.split.map(&:to_i)
exit if data.empty?

idx = 0
t = data[idx]
idx += 1
blocks = []
t.times do
  n1 = data[idx]
  idx += 1
  nums1 = data[idx, n1]
  idx += n1
  n2 = data[idx]
  idx += 1
  nums2 = data[idx, n2]
  idx += n2
  k = data[idx]
  idx += 1
  blocks << "[#{max_number(nums1, nums2, k).join(',')}]"
end

print blocks.join("\n\n")
