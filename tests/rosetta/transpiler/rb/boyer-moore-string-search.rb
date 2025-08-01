# Generated by Mochi transpiler v0.10.54 on 2025-08-02 16:16 +0700
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


class String
  alias each each_char
end

start_mem = _mem()
start = _now()
  def indexOfStr(h, n)
    hlen = h.length
    nlen = n.length
    if nlen == 0
      return 0
    end
    i = 0
    while i <= hlen - nlen
      if h[i..._add(i, nlen)] == n
        return i
      end
      i = _add(i, 1)
    end
    return -1
  end
  def stringSearchSingle(h, n)
    return indexOfStr(h, n)
  end
  def stringSearch(h, n)
    result = []
    start = 0
    hlen = h.length
    nlen = n.length
    while start < hlen
      idx = indexOfStr(h[start...hlen], n)
      if idx >= 0
        result = result + [_add(start, idx)]
        start = _add(_add(start, idx), nlen)
      else
        break
      end
    end
    return result
  end
  def display(nums)
    s = "["
    i = 0
    while i < nums.length
      if i > 0
        s = _add(s, ", ")
      end
      s = _add(s, (nums[i]).to_s)
      i = _add(i, 1)
    end
    s = _add(s, "]")
    return s
  end
  def main()
    texts = ["GCTAGCTCTACGAGTCTA", "GGCTATAATGCGTA", "there would have been a time for such a word", "needle need noodle needle", "DKnuthusesandprogramsanimaginarycomputertheMIXanditsassociatedmachinecodeandassemblylanguages", "Nearby farms grew an acre of alfalfa on the dairy's behalf, with bales of that alfalfa exchanged for milk."]
    patterns = ["TCTA", "TAATAAA", "word", "needle", "and", "alfalfa"]
    i = 0
    while i < texts.length
      puts(_add(_add(_add("text", (_add(i, 1)).to_s), " = "), texts[i]))
      i = _add(i, 1)
    end
    puts("")
    j = 0
    while j < texts.length
      idxs = stringSearch(texts[j], patterns[j])
      puts(_add(_add(_add(_add(_add("Found \"", patterns[j]), "\" in 'text"), (_add(j, 1)).to_s), "' at indexes "), display(idxs)))
      j = _add(j, 1)
    end
  end
  main()
end_time = _now()
end_mem = _mem()
result = {"duration_us" => ((end_time - start) / 1000), "memory_bytes" => (end_mem - start_mem), "name" => "main"}
puts(JSON.pretty_generate(result))
