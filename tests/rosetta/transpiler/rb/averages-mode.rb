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
  $arr1 = [2, 7, 1, 8, 2]
  $counts1 = {}
  $keys1 = []
  $i = 0
  while $i < $arr1.length
    $v = $arr1[$i]
    if $counts1.key?($v)
      $counts1[$v] = _add($counts1[$v], 1)
    else
      $counts1[$v] = 1
      $keys1 = $keys1 + [$v]
    end
    $i = _add($i, 1)
  end
  $max1 = 0
  $i = 0
  while $i < $keys1.length
    $k = $keys1[$i]
    $c = $counts1[$k]
    if $c > $max1
      $max1 = $c
    end
    $i = _add($i, 1)
  end
  $modes1 = []
  $i = 0
  while $i < $keys1.length
    $k = $keys1[$i]
    if $counts1[$k] == $max1
      $modes1 = $modes1 + [$k]
    end
    $i = _add($i, 1)
  end
  puts(($modes1).to_s)
  $arr2 = [2, 7, 1, 8, 2, 8]
  $counts2 = {}
  $keys2 = []
  $i = 0
  while $i < $arr2.length
    $v = $arr2[$i]
    if $counts2.key?($v)
      $counts2[$v] = _add($counts2[$v], 1)
    else
      $counts2[$v] = 1
      $keys2 = $keys2 + [$v]
    end
    $i = _add($i, 1)
  end
  $max2 = 0
  $i = 0
  while $i < $keys2.length
    $k = $keys2[$i]
    $c = $counts2[$k]
    if $c > $max2
      $max2 = $c
    end
    $i = _add($i, 1)
  end
  $modes2 = []
  $i = 0
  while $i < $keys2.length
    $k = $keys2[$i]
    if $counts2[$k] == $max2
      $modes2 = $modes2 + [$k]
    end
    $i = _add($i, 1)
  end
  puts(($modes2).to_s)
end_time = _now()
end_mem = _mem()
result = {"duration_us" => ((end_time - start) / 1000), "memory_bytes" => (end_mem - start_mem), "name" => "main"}
puts(JSON.pretty_generate(result))
