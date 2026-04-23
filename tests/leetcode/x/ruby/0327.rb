def count_range_sum(nums, lower, upper)
  pref = [0]
  nums.each { |x| pref << pref[-1] + x }
  sort = lambda do |lo, hi|
    return 0 if hi - lo <= 1
    mid = (lo + hi) / 2
    ans = sort.call(lo, mid) + sort.call(mid, hi)
    left = lo
    right = lo
    (mid...hi).each do |r|
      left += 1 while left < mid && pref[left] < pref[r] - upper
      right += 1 while right < mid && pref[right] <= pref[r] - lower
      ans += right - left
    end
    pref[lo...hi] = pref[lo...hi].sort
    ans
  end
  sort.call(0, pref.length)
end

data = STDIN.read.split.map(&:to_i)
if data.length > 0
  idx = 0
  t = data[idx]; idx += 1
  out = []
  t.times do
    n = data[idx]; idx += 1
    nums = data[idx, n]; idx += n
    lower = data[idx]; upper = data[idx + 1]; idx += 2
    out << count_range_sum(nums, lower, upper).to_s
  end
  print out.join("\n\n")
end
