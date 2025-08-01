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
  def bernoulli(n)
    a = []
    m = 0
    while m <= n
      a = a + [(1).to_r / (((_add(m, 1))).to_r)]
      j = m
      while j >= 1
        a[j - 1] = ((j).to_r) * (a[j - 1] - a[j])
        j = j - 1
      end
      m = _add(m, 1)
    end
    if n != 1
      return a[0]
    end
    return -a[0]
  end
  def binom(n, k)
    if k < 0 || k > n
      return (0)
    end
    kk = k
    if kk > n - kk
      kk = n - kk
    end
    res = 1
    i = 0
    while i < kk
      res = res * (((n - i)))
      i = _add(i, 1)
      res = res / ((i))
    end
    return res
  end
  def faulhaberRow(p)
    coeffs = []
    i = 0
    while i <= p
      coeffs = coeffs + [(0).to_r]
      i = _add(i, 1)
    end
    j = 0
    sign = -1
    while j <= p
      sign = -sign
      c = (1).to_r / (((_add(p, 1))).to_r)
      if sign < 0
        c = -c
      end
      c = c * ((binom(_add(p, 1), j)).to_r)
      c = c * bernoulli(j)
      coeffs[p - j] = c
      j = _add(j, 1)
    end
    return coeffs
  end
  def ratStr(r)
    s = (r).to_s
    if endsWith(s, "/1")
      return s[0...s.length - 2]
    end
    return s
  end
  def endsWith(s, suf)
    if s.length < suf.length
      return false
    end
    return s[s.length - suf.length...s.length] == suf
  end
  def main()
    p = 0
    while p < 10
      row = faulhaberRow(p)
      line = ""
      idx = 0
      while idx < row.length
        line = _add(line, _padStart(ratStr(row[idx]), 5, " "))
        if idx < row.length - 1
          line = _add(line, "  ")
        end
        idx = _add(idx, 1)
      end
      puts(line)
      p = _add(p, 1)
    end
    puts("")
    k = 17
    coeffs = faulhaberRow(k)
    nn = (1000).to_r
    np = (1).to_r
    sum = (0).to_r
    i = 0
    while i < coeffs.length
      np = np * nn
      sum = _add(sum, coeffs[i] * np)
      i = _add(i, 1)
    end
    puts(ratStr(sum))
  end
  main()
end_time = _now()
end_mem = _mem()
result = {"duration_us" => ((end_time - start) / 1000), "memory_bytes" => (end_mem - start_mem), "name" => "main"}
puts(JSON.pretty_generate(result))
