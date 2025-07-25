# Generated by Mochi transpiler v0.10.41 on 2025-07-26 23:50 +0700
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
  def isNumeric(s)
    if s == "NaN"
      return true
    end
    i = 0
    if s.length == 0
      return false
    end
    if s[0] == "+" || s[0] == "-"
      if s.length == 1
        return false
      end
      i = 1
    end
    digits = false
    dot = false
    while i < s.length
      ch = s[i]
      if ch >= "0" && ch <= "9"
        digits = true
        i = _add(i, 1)
      else
        if ch == "." && dot == false
          dot = true
          i = _add(i, 1)
        else
          if (ch == "e" || ch == "E") && digits
            i = _add(i, 1)
            if i < s.length && (s[i] == "+" || s[i] == "-")
              i = _add(i, 1)
            end
            ed = false
            while i < s.length && s[i] >= "0" && s[i] <= "9"
              ed = true
              i = _add(i, 1)
            end
            return ed && i == s.length
          else
            return false
          end
        end
      end
    end
    return digits
  end
  def main()
    puts("Are these strings numeric?")
    strs = ["1", "3.14", "-100", "1e2", "NaN", "rose"]
    strs.each do |s|
      puts(_add(_add(_add("  ", s), " -> "), (isNumeric(s)).to_s))
    end
  end
  main()
end_time = _now()
end_mem = _mem()
result = {"duration_us" => ((end_time - start) / 1000), "memory_bytes" => (end_mem - start_mem), "name" => "main"}
puts(JSON.pretty_generate(result))
