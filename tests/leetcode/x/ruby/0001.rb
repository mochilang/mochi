def two_sum(nums, target)
  (0...nums.length).each do |i|
    ((i + 1)...nums.length).each do |j|
      return [i, j] if nums[i] + nums[j] == target
    end
  end
  [0, 0]
end

tokens = STDIN.read.split.map(&:to_i)
exit if tokens.empty?

idx = 0
t = tokens[idx]
idx += 1
out = []

t.times do
  n = tokens[idx]
  target = tokens[idx + 1]
  idx += 2
  nums = tokens[idx, n]
  idx += n
  a, b = two_sum(nums, target)
  out << "#{a} #{b}"
end

STDOUT.write(out.join("\n"))
