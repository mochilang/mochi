# Generated by Mochi transpiler v0.10.55 on 2025-08-02 20:27 +0700
require 'json'

$now_seed = 0
$now_seeded = false
s = ENV['MOCHI_NOW_SEED']
if (!s || s == '') && ENV['MOCHI_BENCHMARK']
  s = '1'
end
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


def _input()
  line = STDIN.gets
  line ? line.chomp : ''
end


def _indexOf(s, ch)
  idx = s.index(ch)
  idx ? idx : -1
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
  def indexOf(s, ch)
    i = 0
    while i < s.length
      if s[i..._add(i, 1)] == ch
        return i
      end
      i = _add(i, 1)
    end
    return -1
  end
  def shuffle(xs)
    arr = xs
    i = arr.length - 1
    while i > 0
      j = _now() % (_add(i, 1))
      tmp = arr[i]
      arr[i] = arr[j]
      arr[j] = tmp
      i = i - 1
    end
    return arr
  end
  def main()
    puts("Cows and Bulls")
    puts("Guess four digit number of unique digits in the range 1 to 9.")
    puts("A correct digit but not in the correct place is a cow.")
    puts("A correct digit in the correct place is a bull.")
    digits = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
    digits = shuffle(digits)
    pat = _add(_add(_add(digits[0], digits[1]), digits[2]), digits[3])
    valid = "123456789"
    while true
      puts("Guess: ")
      guess = _input()
      if guess.length != 4
        puts("Please guess a four digit number.")
        next
      end
      cows = 0
      bulls = 0
      seen = ""
      i = 0
      malformed = false
      while i < 4
        cg = guess[i..._add(i, 1)]
        if _indexOf(seen, cg) != (-1)
          puts(_add("Repeated digit: ", cg))
          malformed = true
          break
        end
        seen = _add(seen, cg)
        pos = _indexOf(pat, cg)
        if pos == (-1)
          if _indexOf(valid, cg) == (-1)
            puts(_add("Invalid digit: ", cg))
            malformed = true
            break
          end
        else
          if pos == i
            bulls = _add(bulls, 1)
          else
            cows = _add(cows, 1)
          end
        end
        i = _add(i, 1)
      end
      if malformed
        next
      end
      puts(_add(_add(_add("Cows: ", (cows).to_s), ", bulls: "), (bulls).to_s))
      if bulls == 4
        puts("You got it.")
        break
      end
    end
  end
  main()
end_time = _now()
end_mem = _mem()
result = {"duration_us" => ((end_time - start) / 1000), "memory_bytes" => (end_mem - start_mem), "name" => "main"}
puts(JSON.pretty_generate(result))
