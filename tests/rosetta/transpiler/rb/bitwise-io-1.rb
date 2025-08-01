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
  def pow2(n)
    v = 1
    i = 0
    while i < n
      v = v * 2
      i = _add(i, 1)
    end
    return v
  end
  def lshift(x, n)
    return x * pow2(n)
  end
  def rshift(x, n)
    return x / pow2(n)
  end
  def NewWriter(order)
    return Writer.new(order: order, bits: 0, nbits: 0, data: [])
  end
  def writeBitsLSB(w, c, width)
    w["bits"] = _add(w.bits, lshift(c, w.nbits))
    w["nbits"] = _add(w.nbits, width)
    while w.nbits >= 8
      b = w.bits % 256
      w["data"] = w.data + [b]
      w["bits"] = rshift(w.bits, 8)
      w["nbits"] = w.nbits - 8
    end
    return w
  end
  def writeBitsMSB(w, c, width)
    w["bits"] = _add(w.bits, lshift(c, 32 - width - w.nbits))
    w["nbits"] = _add(w.nbits, width)
    while w.nbits >= 8
      b = rshift(w.bits, 24) % 256
      w["data"] = w.data + [b]
      w["bits"] = (w.bits % pow2(24)) * 256
      w["nbits"] = w.nbits - 8
    end
    return w
  end
  def WriteBits(w, c, width)
    if w.order == "LSB"
      return writeBitsLSB(w, c, width)
    end
    return writeBitsMSB(w, c, width)
  end
  def CloseWriter(w)
    if w.nbits > 0
      if w.order == "MSB"
        w["bits"] = rshift(w.bits, 24)
      end
      w["data"] = w.data + [w.bits % 256]
    end
    w["bits"] = 0
    w["nbits"] = 0
    return w
  end
  def toBinary(n, bits)
    b = ""
    val = n
    i = 0
    while i < bits
      b = _add((val % 2).to_s, b)
      val = val / 2
      i = _add(i, 1)
    end
    return b
  end
  def bytesToBits(bs)
    out = "["
    i = 0
    while i < bs.length
      out = _add(out, toBinary(bs[i], 8))
      if _add(i, 1) < bs.length
        out = _add(out, " ")
      end
      i = _add(i, 1)
    end
    out = _add(out, "]")
    return out
  end
  def ExampleWriter_WriteBits()
    bw = NewWriter("MSB")
    bw = WriteBits(bw, 15, 4)
    bw = WriteBits(bw, 0, 1)
    bw = WriteBits(bw, 19, 5)
    bw = CloseWriter(bw)
    puts(bytesToBits(bw.data))
  end
  Writer = Struct.new(:order, :bits, :nbits, :data, keyword_init: true)
  ExampleWriter_WriteBits()
end_time = _now()
end_mem = _mem()
result = {"duration_us" => ((end_time - start) / 1000), "memory_bytes" => (end_mem - start_mem), "name" => "main"}
puts(JSON.pretty_generate(result))
