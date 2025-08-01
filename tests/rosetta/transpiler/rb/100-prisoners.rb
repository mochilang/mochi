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
  def shuffle(xs)
    arr = xs
    i = 99
    while i > 0
      j = _now() % (_add(i, 1))
      tmp = arr[i]
      arr[i] = arr[j]
      arr[j] = tmp
      i = i - 1
    end
    return arr
  end
  def doTrials(trials, np, strategy)
    pardoned = 0
    t = 0
    while t < trials
      drawers = []
      i = 0
      while i < 100
        drawers = drawers + [i]
        i = _add(i, 1)
      end
      drawers = shuffle(drawers)
      p = 0
      success = true
      while p < np
        found = false
        if strategy == "optimal"
          prev = p
          d = 0
          while d < 50
            this = drawers[prev]
            if this == p
              found = true
              break
            end
            prev = this
            d = _add(d, 1)
          end
        else
          opened = []
          k = 0
          while k < 100
            opened = opened + [false]
            k = _add(k, 1)
          end
          d = 0
          while d < 50
            n = _now() % 100
            while opened[n]
              n = _now() % 100
            end
            opened[n] = true
            if drawers[n] == p
              found = true
              break
            end
            d = _add(d, 1)
          end
        end
        if !found
          success = false
          break
        end
        p = _add(p, 1)
      end
      if success
        pardoned = _add(pardoned, 1)
      end
      t = _add(t, 1)
    end
    rf = ((pardoned).to_f) / ((trials).to_f) * 100.0
    puts(_add(_add(_add(_add(_add(_add("  strategy = ", strategy), "  pardoned = "), (pardoned).to_s), " relative frequency = "), (rf).to_s), "%"))
  end
  def main()
    trials = 1000
    [10, 100].each do |np|
      puts(_add(_add(_add(_add("Results from ", (trials).to_s), " trials with "), (np).to_s), " prisoners:\n"))
      ["random", "optimal"].each do |strat|
        doTrials(trials, np, strat)
      end
    end
  end
  main()
end_time = _now()
end_mem = _mem()
result = {"duration_us" => ((end_time - start) / 1000), "memory_bytes" => (end_mem - start_mem), "name" => "main"}
puts(JSON.pretty_generate(result))
