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
  def padLeft(s, w)
    res = ""
    n = w - s.length
    while n > 0
      res = _add(res, " ")
      n = n - 1
    end
    return _add(res, s)
  end
  $dna = _add(_add(_add(_add(_add(_add(_add(_add(_add(_add("", "CGTAAAAAATTACAACGTCCTTTGGCTATCTCTTAAACTCCTGCTAAATG"), "CTCGTGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTG"), "AGGACAAAGGTCAAGATGGAGCGCATCGAACGCAATAAGGATCATTTGAT"), "GGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTT"), "CGATTCTGCTTATAACACTATGTTCTTATGAAATGGATGTTCTGAGTTGG"), "TCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTATA"), "TTTAATTTTTCTATATAGCGATCTGTATTTAAGCAATTCATTTAGGTTAT"), "CGCCGCGATGCTCGGTTCGGACCGCCAAGCATCTGGCTCCACTGCTAGTG"), "TCCTAAATTTGAATGGCAAACACAAATAAGATTTAGCAATTCGTGTAGAC"), "GACCGGGGACTTGCATGATGGGAGCAGCTTTGTTAAACTACGAACGTAAT")
  puts("SEQUENCE:")
  $le = $dna.length
  $i = 0
  while $i < $le
    $k = _add($i, 50)
    if $k > $le
      $k = $le
    end
    puts(_add(_add(padLeft(($i).to_s, 5), ": "), $dna[$i...$k]))
    $i = _add($i, 50)
  end
  $a = 0
  $c = 0
  $g = 0
  $t = 0
  $idx = 0
  while $idx < $le
    $ch = $dna[$idx..._add($idx, 1)]
    if $ch == "A"
      $a = _add($a, 1)
    else
      if $ch == "C"
        $c = _add($c, 1)
      else
        if $ch == "G"
          $g = _add($g, 1)
        else
          if $ch == "T"
            $t = _add($t, 1)
          end
        end
      end
    end
    $idx = _add($idx, 1)
  end
  puts("")
  puts("BASE COUNT:")
  puts(_add("    A: ", padLeft(($a).to_s, 3)))
  puts(_add("    C: ", padLeft(($c).to_s, 3)))
  puts(_add("    G: ", padLeft(($g).to_s, 3)))
  puts(_add("    T: ", padLeft(($t).to_s, 3)))
  puts("    ------")
  puts(_add("    Σ: ", ($le).to_s))
  puts("    ======")
end_time = _now()
end_mem = _mem()
result = {"duration_us" => ((end_time - start) / 1000), "memory_bytes" => (end_mem - start_mem), "name" => "main"}
puts(JSON.pretty_generate(result))
