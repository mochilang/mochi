# Generated by Mochi transpiler v0.10.42 on 2025-07-28 10:03 +0700
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
  def listString()
    if $head == 0 - 1
      return "<nil>"
    end
    r = _add("[", $nodes[$head]["value"])
    id = ($nodes[$head]["next"]).to_i
    while id != 0 - 1
      r = _add(_add(r, " "), $nodes[id]["value"])
      id = ($nodes[id]["next"]).to_i
    end
    r = _add(r, "]")
    return r
  end
  $nodes = {}
  $head = 0 - 1
  $tail = 0 - 1
  puts(listString())
  $nodes[0] = {"value" => "A", "next" => 0 - 1, "prev" => 0 - 1}
  $head = 0
  $tail = 0
  $nodes[1] = {"value" => "B", "next" => 0 - 1, "prev" => 0}
  $nodes[0]["next"] = 1
  $tail = 1
  puts(listString())
  $nodes[2] = {"value" => "C", "next" => 1, "prev" => 0}
  $nodes[1]["prev"] = 2
  $nodes[0]["next"] = 2
  puts(listString())
  $out = "From tail:"
  $id = $tail
  while $id != 0 - 1
    $out = _add(_add($out, " "), $nodes[$id]["value"])
    $id = ($nodes[$id]["prev"]).to_i
  end
  puts($out)
end_time = _now()
end_mem = _mem()
result = {"duration_us" => ((end_time - start) / 1000), "memory_bytes" => (end_mem - start_mem), "name" => "main"}
puts(JSON.pretty_generate(result))
