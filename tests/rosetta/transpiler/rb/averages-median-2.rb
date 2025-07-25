# Generated by Mochi transpiler v0.10.40 on 2025-07-25 20:07 +0700
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
  def sel(list, k)
    i = 0
    while i <= k
      minIndex = i
      j = _add(i, 1)
      while j < list.length
        if list[j] < list[minIndex]
          minIndex = j
        end
        j = _add(j, 1)
      end
      tmp = list[i]
      list[i] = list[minIndex]
      list[minIndex] = tmp
      i = _add(i, 1)
    end
    return list[k]
  end
  def median(a)
    arr = a
    half = ((arr.length / 2)).to_i
    med = sel(arr, half)
    if arr.length % 2 == 0
      return (_add(med, arr[half - 1])) / 2.0
    end
    return med
  end
  puts((median([3.0, 1.0, 4.0, 1.0])).to_s)
  puts((median([3.0, 1.0, 4.0, 1.0, 5.0])).to_s)
end_time = _now()
end_mem = _mem()
result = {"duration_us" => ((end_time - start) / 1000), "memory_bytes" => (end_mem - start_mem), "name" => "main"}
puts(JSON.pretty_generate(result))
