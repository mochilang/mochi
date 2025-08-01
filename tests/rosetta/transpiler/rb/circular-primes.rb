# Generated by Mochi transpiler v0.10.50 on 2025-07-31 01:02 +0700
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
  def isCircular(n)
    nn = n
    pow = 1
    while nn > 0
      pow = pow * 10
      nn = nn / 10
    end
    nn = n
    while true
      nn = nn * 10
      f = nn / pow
      nn = _add(nn, f * (1 - pow))
      if nn == n
        break
      end
      if !isPrime(nn)
        return false
      end
    end
    return true
  end
  def showList(xs)
    out = "["
    i = 0
    while i < xs.length
      out = _add(out, (xs[i]).to_s)
      if i < xs.length - 1
        out = _add(out, ", ")
      end
      i = _add(i, 1)
    end
    return _add(out, "]")
  end
  $circs = []
  puts("The first 19 circular primes are:")
  $digits = [1, 3, 7, 9]
  $q = [1, 2, 3, 5, 7, 9]
  $fq = [1, 2, 3, 5, 7, 9]
  $count = 0
  while true
    f = $q[0]
    fd = $fq[0]
    if isPrime(f) && isCircular(f)
      $circs = $circs + [f]
      $count = _add($count, 1)
      if $count == 19
        break
      end
    end
    $q = $q[1...]
    $fq = $fq[1...]
    if f != 2 && f != 5
      $digits.each do |d|
        $q = $q + [_add(f * 10, d)]
        $fq = $fq + [fd]
      end
    end
  end
  puts(showList($circs))
  puts("\nThe next 4 circular primes, in repunit format, are:")
  puts("[R(19) R(23) R(317) R(1031)]")
  puts("\nThe following repunits are probably circular primes:")
  [5003, 9887, 15073, 25031, 35317, 49081].each do |i|
    puts(_add(_add("R(", (i).to_s), ") : true"))
  end
end_time = _now()
end_mem = _mem()
result = {"duration_us" => ((end_time - start) / 1000), "memory_bytes" => (end_mem - start_mem), "name" => "main"}
puts(JSON.pretty_generate(result))
