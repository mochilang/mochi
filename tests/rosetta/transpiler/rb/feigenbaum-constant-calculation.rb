# Generated by Mochi transpiler v0.10.54 on 2025-08-02 14:11 +0700
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


def _indexOf(s, ch)
  idx = s.index(ch)
  idx ? idx : -1
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
  def floorf(x)
    y = (x).to_i
    return (y).to_f
  end
  def indexOf(s, ch)
    i = 0
    while i < s.length
      if s[i..._add(i, 1)] == ch
        return i
      end
      i = _add(i, 1)
    end
    return 0 - 1
  end
  def fmt8(x)
    y = floorf(_add(x * 100000000.0, 0.5)) / 100000000.0
    s = (y).to_s
    dot = _indexOf(s, ".")
    if dot == 0 - 1
      s = _add(s, ".00000000")
    else
      decs = s.length - dot - 1
      while decs < 8
        s = _add(s, "0")
        decs = _add(decs, 1)
      end
    end
    return s
  end
  def pad2(x)
    s = (x).to_s
    if s.length < 2
      s = _add(" ", s)
    end
    return s
  end
  def main()
    maxIt = 13
    maxItJ = 10
    a1 = 1.0
    a2 = 0.0
    d1 = 3.2
    puts(" i       d")
    i = 2
    while i <= maxIt
      a = _add(a1, (a1 - a2) / d1)
      j = 1
      while j <= maxItJ
        x = 0.0
        y = 0.0
        k = 1
        limit = pow_int(2, i)
        while k <= limit
          y = 1.0 - 2.0 * y * x
          x = a - x * x
          k = _add(k, 1)
        end
        a = a - x / y
        j = _add(j, 1)
      end
      d = (a1 - a2) / (a - a1)
      puts(_add(_add(pad2(i), "    "), fmt8(d)))
      d1 = d
      a2 = a1
      a1 = a
      i = _add(i, 1)
    end
  end
  def pow_int(base, exp)
    r = 1
    b = base
    e = exp
    while e > 0
      if e % 2 == 1
        r = r * b
      end
      b = b * b
      e = ((e / 2)).to_i
    end
    return r
  end
  main()
end_time = _now()
end_mem = _mem()
result = {"duration_us" => ((end_time - start) / 1000), "memory_bytes" => (end_mem - start_mem), "name" => "main"}
puts(JSON.pretty_generate(result))
