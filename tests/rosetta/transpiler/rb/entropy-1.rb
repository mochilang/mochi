# Generated by Mochi transpiler v0.10.42 on 2025-07-28 10:03 +0700
require 'json'

$now_seed = 0
$now_seeded = false
s = ENV['MOCHI_NOW_SEED']
if s && s != ''
  begin
    $now_seed = Integer(s)
    $now_seeded = true
  rescue StandardError
  end
end
def _now()
  if $now_seeded
    $now_seed = ($now_seed * 1664525 + 1013904223) % 2147483647
    $now_seed
  else
    Process.clock_gettime(Process::CLOCK_MONOTONIC, :nanosecond)
  end
end


require 'objspace'
def _mem()
  ObjectSpace.memsize_of_all
end


def _add(a, b)
  if a.is_a?(Array) && b.is_a?(String)
    a.join + b
  elsif a.is_a?(String) && b.is_a?(Array)
    a + b.join
  else
    a + b
  end
end


def _padStart(s, len, ch)
  s.to_s.rjust(len, ch)
end

start_mem = _mem()
start = _now()
  def log2(x)
    k = 0.0
    v = x
    while v >= 2.0
      v = v / 2.0
      k = _add(k, 1.0)
    end
    while v < 1.0
      v = v * 2.0
      k = k - 1.0
    end
    z = (v - 1.0) / (_add(v, 1.0))
    zpow = z
    sum = z
    i = 3
    while i <= 9
      zpow = zpow * z * z
      sum = _add(sum, zpow / ((i).to_f))
      i = _add(i, 2)
    end
    ln2 = 0.6931471805599453
    return _add(k, 2.0 * sum / ln2)
  end
  def H(data)
    if data == ""
      return 0.0
    end
    counts = {}
    i = 0
    while i < data.length
      ch = data[i..._add(i, 1)]
      if counts.key?(ch)
        counts[ch] = _add(counts[ch], 1)
      else
        counts[ch] = 1
      end
      i = _add(i, 1)
    end
    entropy = 0.0
    l = (data.length).to_f
    counts.keys().each do |ch|
      px = ((counts[ch]).to_f) / l
      if px > 0.0
        entropy = entropy - px * log2(px)
      end
    end
    return entropy
  end
  def main()
    puts((H("1223334444")).to_s)
  end
  main()
end_time = _now()
end_mem = _mem()
result = {"duration_us" => ((end_time - start) / 1000), "memory_bytes" => (end_mem - start_mem), "name" => "main"}
puts(JSON.pretty_generate(result))
