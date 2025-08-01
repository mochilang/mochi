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
  def egyptianDivide(dividend, divisor)
    if dividend < 0 || divisor <= 0
      panic("Invalid argument(s)")
    end
    if dividend < divisor
      return DivResult.new(q: 0, r: dividend)
    end
    powers = [1]
    doublings = [divisor]
    doubling = divisor * 2
    while doubling <= dividend
      powers = powers + [powers[powers.length - 1] * 2]
      doublings = doublings + [doubling]
      doubling = doubling * 2
    end
    ans = 0
    accum = 0
    i = doublings.length - 1
    while i >= 0
      if _add(accum, doublings[i]) <= dividend
        accum = _add(accum, doublings[i])
        ans = _add(ans, powers[i])
        if accum == dividend
          break
        end
      end
      i = i - 1
    end
    return DivResult.new(q: ans, r: dividend - accum)
  end
  def main()
    dividend = 580
    divisor = 34
    res = egyptianDivide(dividend, divisor)
    puts(_add(_add(_add(_add(_add(_add((dividend).to_s, " divided by "), (divisor).to_s), " is "), (res.q).to_s), " with remainder "), (res.r).to_s))
  end
  DivResult = Struct.new(:q, :r, keyword_init: true)
  main()
end_time = _now()
end_mem = _mem()
result = {"duration_us" => ((end_time - start) / 1000), "memory_bytes" => (end_mem - start_mem), "name" => "main"}
puts(JSON.pretty_generate(result))
