PAIRS = [["0", "0"], ["1", "1"], ["6", "9"], ["8", "8"], ["9", "6"]]

def build_nums(n, m)
  return [""] if n == 0
  return ["0", "1", "8"] if n == 1
  mids = build_nums(n - 2, m)
  mids.flat_map do |mid|
    PAIRS.each_with_object([]) do |(a, b), acc|
      next if n == m && a == "0"
      acc << "#{a}#{mid}#{b}"
    end
  end
end

def count_range(low, high)
  ans = 0
  (low.length..high.length).each do |len|
    build_nums(len, len).each do |s|
      next if len == low.length && s < low
      next if len == high.length && s > high
      ans += 1
    end
  end
  ans
end

lines = STDIN.read.lines(chomp: true)
unless lines.empty?
  t = lines[0].to_i
  idx = 1
  out = []
  t.times do
    out << count_range(lines[idx], lines[idx + 1]).to_s
    idx += 2
  end
  puts out
end
