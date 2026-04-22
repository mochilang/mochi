class MedianFinder
  def initialize
    @data = []
  end

  def add_num(num)
    lo = 0
    hi = @data.length
    while lo < hi
      mid = (lo + hi) / 2
      if @data[mid] < num
        lo = mid + 1
      else
        hi = mid
      end
    end
    @data.insert(lo, num)
  end

  def find_median
    n = @data.length
    return @data[n / 2].to_f if n.odd?

    (@data[n / 2 - 1] + @data[n / 2]) / 2.0
  end
end

lines = STDIN.read.lines.map(&:strip).reject(&:empty?)
exit if lines.empty?

t = lines[0].to_i
idx = 1
blocks = []
t.times do
  m = lines[idx].to_i
  idx += 1
  mf = MedianFinder.new
  out = []
  m.times do
    parts = lines[idx].split
    idx += 1
    if parts[0] == "addNum"
      mf.add_num(parts[1].to_i)
    else
      out << format("%.1f", mf.find_median)
    end
  end
  blocks << out.join("\n")
end

print blocks.join("\n\n")
