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
  def pow10(n)
    r = 1.0
    i = 0
    while i < n
      r = r * 10.0
      i = _add(i, 1)
    end
    return r
  end
  def powf(base, exp)
    if exp == 0.5
      guess = base
      i = 0
      while i < 20
        guess = (_add(guess, base / guess)) / 2.0
        i = _add(i, 1)
      end
      return guess
    end
    result = 1.0
    n = (exp).to_i
    i = 0
    while i < n
      result = result * base
      i = _add(i, 1)
    end
    return result
  end
  def formatFloat(f, prec)
    scale = pow10(prec)
    scaled = _add((f * scale), 0.5)
    n = ((scaled).to_i)
    digits = (n).to_s
    while digits.length <= prec
      digits = _add("0", digits)
    end
    intPart = digits[0...digits.length - prec]
    fracPart = digits[digits.length - prec...digits.length]
    return _add(_add(intPart, "."), fracPart)
  end
  def padLeft(s, w)
    res = ""
    n = w - s.length
    while n > 0
      res = _add(res, " ")
      n = n - 1
    end
    return _add(res, s)
  end
  def rowString(row)
    s = "["
    i = 0
    while i < row.length
      s = _add(s, padLeft(formatFloat(row[i], 3), 6))
      if i < row.length - 1
        s = _add(s, " ")
      end
      i = _add(i, 1)
    end
    return _add(s, "] ")
  end
  def printMatrix(heading, m)
    puts(heading)
    i = 0
    while i < m.length
      puts(rowString(m[i]))
      i = _add(i, 1)
    end
  end
  def elementWiseMM(m1, m2, f)
    z = []
    r = 0
    while r < m1.length
      row = []
      c = 0
      while c < m1[r].length
        row = row + [f.call(m1[r][c], m2[r][c])]
        c = _add(c, 1)
      end
      z = z + [row]
      r = _add(r, 1)
    end
    return z
  end
  def elementWiseMS(m, s, f)
    z = []
    r = 0
    while r < m.length
      row = []
      c = 0
      while c < m[r].length
        row = row + [f.call(m[r][c], s)]
        c = _add(c, 1)
      end
      z = z + [row]
      r = _add(r, 1)
    end
    return z
  end
  def add(a, b)
    return _add(a, b)
  end
  def sub(a, b)
    return a - b
  end
  def mul(a, b)
    return a * b
  end
  def div(a, b)
    return a / b
  end
  def exp(a, b)
    return powf(a, b)
  end
  def main()
    m1 = [[3.0, 1.0, 4.0], [1.0, 5.0, 9.0]]
    m2 = [[2.0, 7.0, 1.0], [8.0, 2.0, 8.0]]
    printMatrix("m1:", m1)
    printMatrix("m2:", m2)
    puts("")
    printMatrix("m1 + m2:", elementWiseMM(m1, m2, method(:add)))
    printMatrix("m1 - m2:", elementWiseMM(m1, m2, method(:sub)))
    printMatrix("m1 * m2:", elementWiseMM(m1, m2, method(:mul)))
    printMatrix("m1 / m2:", elementWiseMM(m1, m2, method(:div)))
    printMatrix("m1 ^ m2:", elementWiseMM(m1, m2, method(:exp)))
    puts("")
    s = 0.5
    puts(_add("s: ", (s).to_s))
    printMatrix("m1 + s:", elementWiseMS(m1, s, method(:add)))
    printMatrix("m1 - s:", elementWiseMS(m1, s, method(:sub)))
    printMatrix("m1 * s:", elementWiseMS(m1, s, method(:mul)))
    printMatrix("m1 / s:", elementWiseMS(m1, s, method(:div)))
    printMatrix("m1 ^ s:", elementWiseMS(m1, s, method(:exp)))
  end
  main()
end_time = _now()
end_mem = _mem()
result = {"duration_us" => ((end_time - start) / 1000), "memory_bytes" => (end_mem - start_mem), "name" => "main"}
puts(JSON.pretty_generate(result))
