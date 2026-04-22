def count_digit_one(n)
  total = 0
  m = 1
  while m <= n
    high = n / (m * 10)
    cur = (n / m) % 10
    low = n % m
    if cur == 0
      total += high * m
    elsif cur == 1
      total += high * m + low + 1
    else
      total += (high + 1) * m
    end
    m *= 10
  end
  total
end

lines = STDIN.read.lines(chomp: true)
unless lines.empty?
  t = lines[0].to_i
  puts (0...t).map { |i| count_digit_one((lines[i + 1] || "0").to_i) }
end
