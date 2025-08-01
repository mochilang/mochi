# Generated by Mochi transpiler v0.10.42 on 2025-07-27 23:54 +0700
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
  def pow(base, exp)
    result = 1
    i = 0
    while i < exp
      result = result * base
      i = _add(i, 1)
    end
    return result
  end
  def ackermann2(m, n)
    if m == 0
      return _add(n, 1)
    end
    if m == 1
      return _add(n, 2)
    end
    if m == 2
      return _add(2 * n, 3)
    end
    if m == 3
      return 8 * 2 ** n - 3
    end
    if n == 0
      return ackermann2(m - 1, 1)
    end
    return ackermann2(m - 1, ackermann2(m, n - 1))
  end
  def main()
    puts(_add("A(0, 0) = ", (ackermann2(0, 0)).to_s))
    puts(_add("A(1, 2) = ", (ackermann2(1, 2)).to_s))
    puts(_add("A(2, 4) = ", (ackermann2(2, 4)).to_s))
    puts(_add("A(3, 4) = ", (ackermann2(3, 4)).to_s))
  end
  main()
end_time = _now()
end_mem = _mem()
result = {"duration_us" => ((end_time - start) / 1000), "memory_bytes" => (end_mem - start_mem), "name" => "main"}
puts(JSON.pretty_generate(result))
