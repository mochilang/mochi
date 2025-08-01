# Generated by Mochi transpiler v0.10.42 on 2025-07-28 00:41 +0700
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
  def initGrid(size)
    g = []
    y = 0
    while y < size
      row = []
      x = 0
      while x < size
        row = row + [" "]
        x = _add(x, 1)
      end
      g = g + [row]
      y = _add(y, 1)
    end
    return g
  end
  def set(g, x, y)
    if x >= 0 && x < g[0].length && y >= 0 && y < g.length
      g[y][x] = "#"
    end
  end
  def circle(r)
    size = _add(r * 2, 1)
    g = initGrid(size)
    x = r
    y = 0
    err = 1 - r
    while y <= x
      set(g, _add(r, x), _add(r, y))
      set(g, _add(r, y), _add(r, x))
      set(g, r - x, _add(r, y))
      set(g, r - y, _add(r, x))
      set(g, r - x, r - y)
      set(g, r - y, r - x)
      set(g, _add(r, x), r - y)
      set(g, _add(r, y), r - x)
      y = _add(y, 1)
      if err < 0
        err = _add(_add(err, 2 * y), 1)
      else
        x = x - 1
        err = _add(_add(err, 2 * (y - x)), 1)
      end
    end
    return g
  end
  def trimRight(row)
    end_ = row.length
    while end_ > 0 && row[end_ - 1] == " "
      end_ = end_ - 1
    end
    s = ""
    i = 0
    while i < end_
      s = _add(s, row[i])
      i = _add(i, 1)
    end
    return s
  end
  $g = circle(10)
  $g.each do |row|
    puts(trimRight(row))
  end
end_time = _now()
end_mem = _mem()
result = {"duration_us" => ((end_time - start) / 1000), "memory_bytes" => (end_mem - start_mem), "name" => "main"}
puts(JSON.pretty_generate(result))
