def candy(ratings)
  n = ratings.length
  candies = Array.new(n, 1)
  (1...n).each do |i|
    candies[i] = candies[i - 1] + 1 if ratings[i] > ratings[i - 1]
  end
  (n - 2).downto(0) do |i|
    if ratings[i] > ratings[i + 1]
      candies[i] = [candies[i], candies[i + 1] + 1].max
    end
  end
  candies.sum
end

lines = STDIN.read.split(/\r?\n/)
exit if lines.empty? || lines[0].empty?
tc = lines[0].to_i
idx = 1
out = []
tc.times do
  n = lines[idx].to_i
  idx += 1
  ratings = lines[idx, n].map(&:to_i)
  idx += n
  out << candy(ratings).to_s
end
STDOUT.write(out.join("\n\n"))
