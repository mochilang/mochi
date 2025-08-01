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
  def randInt(s, n)
    next_ = (_add(s * 1664525, 1013904223)) % 2147483647
    return [next_, next_ % n]
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
  def makeSeq(s, le)
    bases = "ACGT"
    out = ""
    i = 0
    while i < le
      r = randInt(s, 4)
      s = r[0]
      idx = (r[1]).to_i
      out = _add(out, bases[idx..._add(idx, 1)])
      i = _add(i, 1)
    end
    return [s, out]
  end
  def mutate(s, dna, w)
    bases = "ACGT"
    le = dna.length
    r = randInt(s, le)
    s = r[0]
    p = (r[1]).to_i
    r = randInt(s, 300)
    s = r[0]
    x = (r[1]).to_i
    arr = []
    i = 0
    while i < le
      arr = arr + [dna[i..._add(i, 1)]]
      i = _add(i, 1)
    end
    if x < w[0]
      r = randInt(s, 4)
      s = r[0]
      idx = (r[1]).to_i
      b = bases[idx..._add(idx, 1)]
      puts(_add(_add(_add(_add(_add(_add("  Change @", padLeft((p).to_s, 3)), " '"), arr[p]), "' to '"), b), "'"))
      arr[p] = b
    else
      if x < _add(w[0], w[1])
        puts(_add(_add(_add(_add("  Delete @", padLeft((p).to_s, 3)), " '"), arr[p]), "'"))
        j = p
        while j < arr.length - 1
          arr[j] = arr[_add(j, 1)]
          j = _add(j, 1)
        end
        arr = arr[0...arr.length - 1]
      else
        r = randInt(s, 4)
        s = r[0]
        idx2 = (r[1]).to_i
        b = bases[idx2..._add(idx2, 1)]
        arr = arr + [""]
        j = arr.length - 1
        while j > p
          arr[j] = arr[j - 1]
          j = j - 1
        end
        puts(_add(_add(_add(_add("  Insert @", padLeft((p).to_s, 3)), " '"), b), "'"))
        arr[p] = b
      end
    end
    out = ""
    i = 0
    while i < arr.length
      out = _add(out, arr[i])
      i = _add(i, 1)
    end
    return [s, out]
  end
  def prettyPrint(dna, rowLen)
    puts("SEQUENCE:")
    le = dna.length
    i = 0
    while i < le
      k = _add(i, rowLen)
      if k > le
        k = le
      end
      puts(_add(_add(padLeft((i).to_s, 5), ": "), dna[i...k]))
      i = _add(i, rowLen)
    end
    a = 0
    c = 0
    g = 0
    t = 0
    idx = 0
    while idx < le
      ch = dna[idx..._add(idx, 1)]
      if ch == "A"
        a = _add(a, 1)
      else
        if ch == "C"
          c = _add(c, 1)
        else
          if ch == "G"
            g = _add(g, 1)
          else
            if ch == "T"
              t = _add(t, 1)
            end
          end
        end
      end
      idx = _add(idx, 1)
    end
    puts("")
    puts("BASE COUNT:")
    puts(_add("    A: ", padLeft((a).to_s, 3)))
    puts(_add("    C: ", padLeft((c).to_s, 3)))
    puts(_add("    G: ", padLeft((g).to_s, 3)))
    puts(_add("    T: ", padLeft((t).to_s, 3)))
    puts("    ------")
    puts(_add("    Σ: ", (le).to_s))
    puts("    ======")
  end
  def wstring(w)
    return _add(_add(_add(_add(_add(_add("  Change: ", (w[0]).to_s), "\n  Delete: "), (w[1]).to_s), "\n  Insert: "), (w[2]).to_s), "\n")
  end
  def main()
    seed = 1
    res = makeSeq(seed, 250)
    seed = res[0]
    dna = (res[1]).to_s
    prettyPrint(dna, 50)
    muts = 10
    w = [100, 100, 100]
    puts("\nWEIGHTS (ex 300):")
    puts(wstring(w))
    puts(_add(_add("MUTATIONS (", (muts).to_s), "):"))
    i = 0
    while i < muts
      res = mutate(seed, dna, w)
      seed = res[0]
      dna = (res[1]).to_s
      i = _add(i, 1)
    end
    puts("")
    prettyPrint(dna, 50)
  end
  main()
end_time = _now()
end_mem = _mem()
result = {"duration_us" => ((end_time - start) / 1000), "memory_bytes" => (end_mem - start_mem), "name" => "main"}
puts(JSON.pretty_generate(result))
