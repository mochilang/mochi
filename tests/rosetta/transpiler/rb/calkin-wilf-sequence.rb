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


def _repeat(s, n)
  s * n.to_i
end


def parseIntStr(str, base = 10)
  str.to_i(base)
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
  def bigrat(a, b)
    return ((a).to_r) / ((b).to_r)
  end
  def calkinWilf(n)
    seq = []
    seq = seq + [bigrat(1, 1)]
    i = 1
    while i < n
      prev = seq[i - 1]
      a = prev.numerator()
      b = prev.denominator()
      f = a / b
      t = bigrat(f, 1)
      t = t * ((2).to_r)
      t = t - prev
      t = _add(t, ((1).to_r))
      t = ((1).to_r) / t
      seq = seq + [t]
      i = _add(i, 1)
    end
    return seq
  end
  def toContinued(r)
    a = r.numerator()
    b = r.denominator()
    res = []
    while true
      res = res + [((a / b)).to_i]
      t = a % b
      a = b
      b = t
      if a == 1
        break
      end
    end
    if res.length % 2 == 0
      res[res.length - 1] = res[res.length - 1] - 1
      res = res + [1]
    end
    return res
  end
  def termNumber(cf)
    b = ""
    d = "1"
    cf.each do |n|
      b = _add(_repeat(d, n), b)
      if d == "1"
        d = "0"
      else
        d = "1"
      end
    end
    return parseIntStr(b, 2)
  end
  def commatize(n)
    s = (n).to_s
    out = ""
    i = 0
    cnt = 0
    neg = false
    if s[0...1] == "-"
      neg = true
      s = s[1...]
    end
    i = s.length - 1
    while i >= 0
      out = _add(s[i..._add(i, 1)], out)
      cnt = _add(cnt, 1)
      if cnt == 3 && i != 0
        out = _add(",", out)
        cnt = 0
      end
      i = i - 1
    end
    if neg
      out = _add("-", out)
    end
    return out
  end
  def main()
    cw = calkinWilf(20)
    puts("The first 20 terms of the Calkin-Wilf sequnence are:")
    i = 0
    while i < 20
      r = cw[i]
      s = (r.numerator()).to_s
      if r.denominator() != 1
        s = _add(_add(s, "/"), (r.denominator()).to_s)
      end
      puts(_add(_add(_padStart((_add(i, (1).to_i)), 2, " "), ": "), s))
      i = _add(i, 1)
    end
    r = bigrat(83116, 51639)
    cf = toContinued(r)
    tn = termNumber(cf)
    puts(_add(_add(_add(_add(_add(_add("", (r.numerator()).to_s), "/"), (r.denominator()).to_s), " is the "), commatize(tn)), "th term of the sequence."))
  end
  main()
end_time = _now()
end_mem = _mem()
result = {"duration_us" => ((end_time - start) / 1000), "memory_bytes" => (end_mem - start_mem), "name" => "main"}
puts(JSON.pretty_generate(result))
