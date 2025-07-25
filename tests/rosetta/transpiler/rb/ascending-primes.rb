# Generated by Mochi transpiler v0.10.40 on 2025-07-25 19:40 +0700
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
  s.rjust(len, ch)
end

start_mem = _mem()
start = _now()
  def isPrime(n)
    if n < 2
      return false
    end
    if n % 2 == 0
      return n == 2
    end
    if n % 3 == 0
      return n == 3
    end
    d = 5
    while d * d <= n
      if n % d == 0
        return false
      end
      d = _add(d, 2)
      if n % d == 0
        return false
      end
      d = _add(d, 4)
    end
    return true
  end
  $asc = []
  def gen(first, cand, digits)
    if digits == 0
      if isPrime(cand)
        $asc = _add($asc, [cand])
      end
      return
    end
    i = first
    while i < 10
      gen(_add(i, 1), _add(cand * 10, i), digits - 1)
      i = _add(i, 1)
    end
  end
  def pad(n, width)
    s = (n).to_s
    while s.length < width
      s = _add(" ", s)
    end
    return s
  end
  def main()
    digits = 1
    while digits < 10
      gen(1, 0, digits)
      digits = _add(digits, 1)
    end
    puts(_add(_add("There are ", ($asc.length).to_s), " ascending primes, namely:"))
    i = 0
    line = ""
    while i < $asc.length
      line = _add(_add(line, pad($asc[i], 8)), " ")
      if (_add(i, 1)) % 10 == 0
        puts(line[0...line.length - 1])
        line = ""
      end
      i = _add(i, 1)
    end
    if line.length > 0
      puts(line[0...line.length - 1])
    end
  end
  main()
end_time = _now()
end_mem = _mem()
result = {"duration_us" => ((end_time - start) / 1000), "memory_bytes" => (end_mem - start_mem), "name" => "main"}
puts(JSON.pretty_generate(result))
