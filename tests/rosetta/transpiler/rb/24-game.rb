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


def _input()
  line = STDIN.gets
  line ? line.chomp : ''
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
  def randDigit()
    return _add((_now() % 9), 1)
  end
  def main()
    digits = []
    (0...4).each do |i|
      digits = digits + [randDigit()]
    end
    numstr = ""
    (0...4).each do |i|
      numstr = _add(numstr, (digits[i]).to_s)
    end
    puts(_add(_add("Your numbers: ", numstr), "\n"))
    puts("Enter RPN: ")
    expr = _input()
    if expr.length != 7
      puts("invalid. expression length must be 7. (4 numbers, 3 operators, no spaces)")
      return
    end
    stack = []
    i = 0
    valid = true
    while i < expr.length
      ch = expr[i..._add(i, 1)]
      if ch >= "0" && ch <= "9"
        if digits.length == 0
          puts("too many numbers.")
          return
        end
        j = 0
        while digits[j] != (ch).to_i - ("0").to_i
          j = _add(j, 1)
          if j == digits.length
            puts("wrong numbers.")
            return
          end
        end
        digits = _add(digits[...j], digits[_add(j, 1)...])
        stack = stack + [((ch).to_i - ("0").to_i).to_f]
      else
        if stack.length < 2
          puts("invalid expression syntax.")
          valid = false
          break
        end
        b = stack[stack.length - 1]
        a = stack[stack.length - 2]
        if ch == "+"
          stack[stack.length - 2] = _add(a, b)
        else
          if ch == "-"
            stack[stack.length - 2] = a - b
          else
            if ch == "*"
              stack[stack.length - 2] = a * b
            else
              if ch == "/"
                stack[stack.length - 2] = a / b
              else
                puts(_add(ch, " invalid."))
                valid = false
                break
              end
            end
          end
        end
        stack = stack[...stack.length - 1]
      end
      i = _add(i, 1)
    end
    if valid
      if abs.call(stack[0] - 24.0) > 1e-06
        puts(_add(_add("incorrect. ", (stack[0]).to_s), " != 24"))
      else
        puts("correct.")
      end
    end
  end
  main()
end_time = _now()
end_mem = _mem()
result = {"duration_us" => ((end_time - start) / 1000), "memory_bytes" => (end_mem - start_mem), "name" => "main"}
puts(JSON.pretty_generate(result))
