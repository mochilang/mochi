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
  def sameDigits(n, b)
    f = n % b
    n = ((n / b)).to_i
    while n > 0
      if n % b != f
        return false
      end
      n = ((n / b)).to_i
    end
    return true
  end
  def isBrazilian(n)
    if n < 7
      return false
    end
    if n % 2 == 0 && n >= 8
      return true
    end
    b = 2
    while b < n - 1
      if sameDigits(n, b)
        return true
      end
      b = _add(b, 1)
    end
    return false
  end
  def isPrime(n)
    if n < 2
      return false
    end
    if n % 2 == 0
      return n == 2
    end
    if n % 3 == 0
      return n == 3
    end
    d = 5
    while d * d <= n
      if n % d == 0
        return false
      end
      d = _add(d, 2)
      if n % d == 0
        return false
      end
      d = _add(d, 4)
    end
    return true
  end
  def main()
    kinds = [" ", " odd ", " prime "]
    kinds.each do |kind|
      puts(_add(_add("First 20", kind), "Brazilian numbers:"))
      c = 0
      n = 7
      while true
        if isBrazilian(n)
          puts(_add((n).to_s, " "))
          c = _add(c, 1)
          if c == 20
            puts("\n")
            break
          end
        end
        if kind == " "
          n = _add(n, 1)
        else
          if kind == " odd "
            n = _add(n, 2)
          else
            while true
              n = _add(n, 2)
              if isPrime(n)
                break
              end
            end
          end
        end
      end
    end
    n = 7
    c = 0
    while c < 100000
      if isBrazilian(n)
        c = _add(c, 1)
      end
      n = _add(n, 1)
    end
    puts(_add("The 100,000th Brazilian number: ", (n - 1).to_s))
  end
  main()
end_time = _now()
end_mem = _mem()
result = {"duration_us" => ((end_time - start) / 1000), "memory_bytes" => (end_mem - start_mem), "name" => "main"}
puts(JSON.pretty_generate(result))
