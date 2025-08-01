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


def _repeat(s, n)
  s * n.to_i
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
  def repeat(s, n)
    out = ""
    i = 0
    while i < n
      out = _add(out, s)
      i = _add(i, 1)
    end
    return out
  end
  def reverseStr(s)
    out = ""
    i = s.length - 1
    while i >= 0
      out = _add(out, s[i..._add(i, 1)])
      i = i - 1
    end
    return out
  end
  $records = [_repeat("abcdefgh", 10), _repeat("ijklmnop", 10), _repeat("qrstuvwx", 10)]
  $records.each do |r|
    puts(reverseStr(r))
  end
end_time = _now()
end_mem = _mem()
result = {"duration_us" => ((end_time - start) / 1000), "memory_bytes" => (end_mem - start_mem), "name" => "main"}
puts(JSON.pretty_generate(result))
