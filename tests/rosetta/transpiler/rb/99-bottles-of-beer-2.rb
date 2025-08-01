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
  def fields(s)
    words = []
    cur = ""
    i = 0
    while i < s.length
      ch = s[i..._add(i, 1)]
      if ch == " " || ch == "\n" || ch == "\t"
        if cur.length > 0
          words = words + [cur]
          cur = ""
        end
      else
        cur = _add(cur, ch)
      end
      i = _add(i, 1)
    end
    if cur.length > 0
      words = words + [cur]
    end
    return words
  end
  def join(xs, sep)
    res = ""
    i = 0
    while i < xs.length
      if i > 0
        res = _add(res, sep)
      end
      res = _add(res, xs[i])
      i = _add(i, 1)
    end
    return res
  end
  def numberName(n)
    small = ["no", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
    tens = ["ones", "ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]
    if n < 0
      return ""
    end
    if n < 20
      return small[n]
    end
    if n < 100
      t = tens[((n / 10)).to_i]
      s = n % 10
      if s > 0
        t = _add(_add(t, " "), small[s])
      end
      return t
    end
    return ""
  end
  def pluralizeFirst(s, n)
    if n == 1
      return s
    end
    w = fields(s)
    if w.length > 0
      w[0] = _add(w[0], "s")
    end
    return join(w, " ")
  end
  def randInt(seed, n)
    next_ = (_add(seed * 1664525, 1013904223)) % 2147483647
    return next_ % n
  end
  def slur(p, d)
    if p.length <= 2
      return p
    end
    a = []
    i = 1
    while i < p.length - 1
      a = a + [p[i..._add(i, 1)]]
      i = _add(i, 1)
    end
    idx = a.length - 1
    seed = d
    while idx >= 1
      seed = (_add(seed * 1664525, 1013904223)) % 2147483647
      if seed % 100 >= d
        j = seed % (_add(idx, 1))
        tmp = a[idx]
        a[idx] = a[j]
        a[j] = tmp
      end
      idx = idx - 1
    end
    s = p[0...1]
    k = 0
    while k < a.length
      s = _add(s, a[k])
      k = _add(k, 1)
    end
    s = _add(s, p[p.length - 1...p.length])
    w = fields(s)
    return join(w, " ")
  end
  def main()
    i = 99
    while i > 0
      puts(_add(_add(_add(_add(slur(numberName(i), i), " "), pluralizeFirst(slur("bottle of", i), i)), " "), slur("beer on the wall", i)))
      puts(_add(_add(_add(_add(slur(numberName(i), i), " "), pluralizeFirst(slur("bottle of", i), i)), " "), slur("beer", i)))
      puts(_add(_add(_add(_add(slur("take one", i), " "), slur("down", i)), " "), slur("pass it around", i)))
      puts(_add(_add(_add(_add(slur(numberName(i - 1), i), " "), pluralizeFirst(slur("bottle of", i), i - 1)), " "), slur("beer on the wall", i)))
      i = i - 1
    end
  end
  main()
end_time = _now()
end_mem = _mem()
result = {"duration_us" => ((end_time - start) / 1000), "memory_bytes" => (end_mem - start_mem), "name" => "main"}
puts(JSON.pretty_generate(result))
