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
  def bigTrim(a)
    n = a.length
    while n > 1 && a[n - 1] == 0
      a = a[0...n - 1]
      n = n - 1
    end
    return a
  end
  def bigFromInt(x)
    if x == 0
      return [0]
    end
    digits = []
    n = x
    while n > 0
      digits = digits + [n % 10]
      n = n / 10
    end
    return digits
  end
  def bigAdd(a, b)
    res = []
    carry = 0
    i = 0
    while i < a.length || i < b.length || carry > 0
      av = 0
      if i < a.length
        av = a[i]
      end
      bv = 0
      if i < b.length
        bv = b[i]
      end
      s = _add(_add(av, bv), carry)
      res = res + [s % 10]
      carry = s / 10
      i = _add(i, 1)
    end
    return bigTrim(res)
  end
  def bigSub(a, b)
    res = []
    borrow = 0
    i = 0
    while i < a.length
      av = a[i]
      bv = 0
      if i < b.length
        bv = b[i]
      end
      diff = av - bv - borrow
      if diff < 0
        diff = _add(diff, 10)
        borrow = 1
      else
        borrow = 0
      end
      res = res + [diff]
      i = _add(i, 1)
    end
    return bigTrim(res)
  end
  def bigToString(a)
    s = ""
    i = a.length - 1
    while i >= 0
      s = _add(s, (a[i]).to_s)
      i = i - 1
    end
    return s
  end
  def minInt(a, b)
    if a < b
      return a
    else
      return b
    end
  end
  def cumu(n)
    cache = [[bigFromInt(1)]]
    y = 1
    while y <= n
      row = [bigFromInt(0)]
      x = 1
      while x <= y
        val = cache[y - x][minInt(x, y - x)]
        row = row + [bigAdd(row[row.length - 1], val)]
        x = _add(x, 1)
      end
      cache = cache + [row]
      y = _add(y, 1)
    end
    return cache[n]
  end
  def row(n)
    e = cumu(n)
    out = []
    i = 0
    while i < n
      diff = bigSub(e[_add(i, 1)], e[i])
      out = out + [bigToString(diff)]
      i = _add(i, 1)
    end
    return out
  end
  puts("rows:")
  $x = 1
  while $x < 11
    $r = row($x)
    $line = ""
    $i = 0
    while $i < $r.length
      $line = _add(_add(_add($line, " "), $r[$i]), " ")
      $i = _add($i, 1)
    end
    puts($line)
    $x = _add($x, 1)
  end
  puts("")
  puts("sums:")
  [23, 123, 1234].each do |num|
    $r = cumu(num)
    puts(_add(_add((num).to_s, " "), bigToString($r[$r.length - 1])))
  end
end_time = _now()
end_mem = _mem()
result = {"duration_us" => ((end_time - start) / 1000), "memory_bytes" => (end_mem - start_mem), "name" => "main"}
puts(JSON.pretty_generate(result))
