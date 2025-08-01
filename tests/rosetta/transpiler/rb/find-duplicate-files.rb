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
  def findDuplicates(fs, paths)
    seen = {}
    dups = []
    paths.each do |path|
      content = fs[path]
      if seen.key?(content)
        dups = dups + [[seen[content], path]]
      else
        seen[content] = path
      end
    end
    return dups
  end
  def main()
    fs = {"a.txt" => "hello", "b.txt" => "world", "c.txt" => "hello", "d.txt" => "foo", "e.txt" => "world"}
    paths = ["a.txt", "b.txt", "c.txt", "d.txt", "e.txt"]
    dups = findDuplicates(fs, paths)
    dups.each do |pair|
      puts(_add(_add(pair[0], " <==> "), pair[1]))
    end
  end
  main()
end_time = _now()
end_mem = _mem()
result = {"duration_us" => ((end_time - start) / 1000), "memory_bytes" => (end_mem - start_mem), "name" => "main"}
puts(JSON.pretty_generate(result))
