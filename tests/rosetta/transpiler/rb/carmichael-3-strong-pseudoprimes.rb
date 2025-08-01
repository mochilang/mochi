# Generated by Mochi transpiler v0.10.50 on 2025-07-31 07:51 +0700
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
  def mod(n, m)
    return (_add((n % m), m)) % m
  end
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
  def pad(n, width)
    s = (n).to_s
    while s.length < width
      s = _add(" ", s)
    end
    return s
  end
  def carmichael(p1)
    (2...p1).each do |h3|
      (1...(_add(h3, p1))).each do |d|
        if ((_add(h3, p1)) * (p1 - 1)) % d == 0 && mod(-p1 * p1, h3) == d % h3
          p2 = _add(1, ((p1 - 1) * (_add(h3, p1)) / d))
          if !isPrime(p2)
            next
          end
          p3 = _add(1, (p1 * p2 / h3))
          if !isPrime(p3)
            next
          end
          if (p2 * p3) % (p1 - 1) != 1
            next
          end
          c = p1 * p2 * p3
          puts(_add(_add(_add(_add(_add(_add(pad(p1, 2), "   "), pad(p2, 4)), "   "), pad(p3, 5)), "     "), (c).to_s))
        end
      end
    end
  end
  puts("The following are Carmichael munbers for p1 <= 61:\n")
  puts("p1     p2      p3     product")
  puts("==     ==      ==     =======")
  (2...62).each do |p1|
    if isPrime(p1)
      carmichael(p1)
    end
  end
end_time = _now()
end_mem = _mem()
result = {"duration_us" => ((end_time - start) / 1000), "memory_bytes" => (end_mem - start_mem), "name" => "main"}
puts(JSON.pretty_generate(result))
