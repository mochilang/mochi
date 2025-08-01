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
  def sortRunes(s)
    arr = []
    i = 0
    while i < s.length
      arr = arr + [s[i..._add(i, 1)]]
      i = _add(i, 1)
    end
    n = arr.length
    m = 0
    while m < n
      j = 0
      while j < n - 1
        if arr[j] > arr[_add(j, 1)]
          tmp = arr[j]
          arr[j] = arr[_add(j, 1)]
          arr[_add(j, 1)] = tmp
        end
        j = _add(j, 1)
      end
      m = _add(m, 1)
    end
    out = ""
    i = 0
    while i < n
      out = _add(out, arr[i])
      i = _add(i, 1)
    end
    return out
  end
  def deranged(a, b)
    if a.length != b.length
      return false
    end
    i = 0
    while i < a.length
      if a[i..._add(i, 1)] == b[i..._add(i, 1)]
        return false
      end
      i = _add(i, 1)
    end
    return true
  end
  def main()
    words = ["constitutionalism", "misconstitutional"]
    m = {}
    bestLen = 0
    w1 = ""
    w2 = ""
    words.each do |w|
      if w.length <= bestLen
        next
      end
      k = sortRunes(w)
      if !(m.key?(k))
        m[k] = [w]
        next
      end
      m[k].each do |c|
        if deranged(w, c)
          bestLen = w.length
          w1 = c
          w2 = w
          break
        end
      end
      m[k] = m[k] + [w]
    end
    puts(_add(_add(_add(_add(w1, " "), w2), " : Length "), (bestLen).to_s))
  end
  main()
end_time = _now()
end_mem = _mem()
result = {"duration_us" => ((end_time - start) / 1000), "memory_bytes" => (end_mem - start_mem), "name" => "main"}
puts(JSON.pretty_generate(result))
