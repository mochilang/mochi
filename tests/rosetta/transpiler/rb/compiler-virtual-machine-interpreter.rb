# Generated by Mochi transpiler v0.10.55 on 2025-08-02 16:57 +0700
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


def _split(s, sep)
  s.to_s.split(sep.to_s)
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


class String
  alias each each_char
end

start_mem = _mem()
start = _now()
  def parseIntStr(str)
    i = 0
    neg = false
    if str.length > 0 && str[0...1] == "-"
      neg = true
      i = 1
    end
    n = 0
    digits = {"0" => 0, "1" => 1, "2" => 2, "3" => 3, "4" => 4, "5" => 5, "6" => 6, "7" => 7, "8" => 8, "9" => 9}
    while i < str.length
      n = _add(n * 10, digits[str[i..._add(i, 1)]])
      i = _add(i, 1)
    end
    if neg
      n = -n
    end
    return n
  end
  def fields(s)
    words = []
    cur = ""
    i = 0
    while i < s.length
      ch = s[i..._add(i, 1)]
      if ch == " " || ch == "\t" || ch == "\n"
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
  def unescape(s)
    out = ""
    i = 0
    while i < s.length
      if s[i..._add(i, 1)] == "\\" && _add(i, 1) < s.length
        c = s[_add(i, 1)..._add(i, 2)]
        if c == "n"
          out = _add(out, "\n")
          i = _add(i, 2)
          next
        else
          if c == "\\"
            out = _add(out, "\\")
            i = _add(i, 2)
            next
          end
        end
      end
      out = _add(out, s[i..._add(i, 1)])
      i = _add(i, 1)
    end
    return out
  end
  def parseProgram(src)
    lines = _split(src, "\n")
    header = fields(lines[0])
    dataSize = parseIntStr(header[1])
    nStrings = parseIntStr(header[3])
    stringPool = []
    i = 1
    while i <= nStrings
      s = lines[i]
      if s.length > 0
        stringPool = stringPool + [unescape(s[1...s.length - 1])]
      end
      i = _add(i, 1)
    end
    code = []
    addrMap = {}
    while i < lines.length
      line = trim(lines[i])
      if line.length == 0
        break
      end
      parts = fields(line)
      addr = parseIntStr(parts[0])
      op = parts[1]
      arg = 0
      if op == "push"
        arg = parseIntStr(parts[2])
      else
        if op == "fetch" || op == "store"
          arg = parseIntStr(parts[2][1...parts[2].length - 1])
        else
          if op == "jmp" || op == "jz"
            arg = parseIntStr(parts[3])
          end
        end
      end
      code = code + [{"addr" => addr, "op" => op, "arg" => arg}]
      addrMap[addr] = code.length - 1
      i = _add(i, 1)
    end
    return {"dataSize" => dataSize, "strings" => stringPool, "code" => code, "addrMap" => addrMap}
  end
  def runVM(prog)
    data = []
    i = 0
    while i < prog["dataSize"]
      data = data + [0]
      i = _add(i, 1)
    end
    stack = []
    pc = 0
    code = prog["code"]
    addrMap = prog["addrMap"]
    pool = prog["strings"]
    line = ""
    while pc < code.length
      inst = code[pc]
      op = inst["op"]
      arg = inst["arg"]
      if op == "push"
        stack = stack + [arg]
        pc = _add(pc, 1)
        next
      end
      if op == "store"
        data[arg] = stack[stack.length - 1]
        stack = stack[0...stack.length - 1]
        pc = _add(pc, 1)
        next
      end
      if op == "fetch"
        stack = stack + [data[arg]]
        pc = _add(pc, 1)
        next
      end
      if op == "add"
        stack[stack.length - 2] = _add(stack[stack.length - 2], stack[stack.length - 1])
        stack = stack[0...stack.length - 1]
        pc = _add(pc, 1)
        next
      end
      if op == "lt"
        v = 0
        if stack[stack.length - 2] < stack[stack.length - 1]
          v = 1
        end
        stack[stack.length - 2] = v
        stack = stack[0...stack.length - 1]
        pc = _add(pc, 1)
        next
      end
      if op == "jz"
        v = stack[stack.length - 1]
        stack = stack[0...stack.length - 1]
        if v == 0
          pc = addrMap[arg]
        else
          pc = _add(pc, 1)
        end
        next
      end
      if op == "jmp"
        pc = addrMap[arg]
        next
      end
      if op == "prts"
        s = pool[stack[stack.length - 1]]
        stack = stack[0...stack.length - 1]
        if s != "\n"
          line = _add(line, s)
        end
        pc = _add(pc, 1)
        next
      end
      if op == "prti"
        line = _add(line, (stack[stack.length - 1]).to_s)
        puts(line)
        line = ""
        stack = stack[0...stack.length - 1]
        pc = _add(pc, 1)
        next
      end
      if op == "halt"
        break
      end
      pc = _add(pc, 1)
    end
  end
  def trim(s)
    start = 0
    while start < s.length && (s[start..._add(start, 1)] == " " || s[start..._add(start, 1)] == "\t")
      start = _add(start, 1)
    end
    end_ = s.length
    while end_ > start && (s[end_ - 1...end_] == " " || s[end_ - 1...end_] == "\t")
      end_ = end_ - 1
    end
    return s[start...end_]
  end
  def split(s, sep)
    parts = []
    cur = ""
    i = 0
    while i < s.length
      if sep.length > 0 && _add(i, sep.length) <= s.length && s[i..._add(i, sep.length)] == sep
        parts = parts + [cur]
        cur = ""
        i = _add(i, sep.length)
      else
        cur = _add(cur, s[i..._add(i, 1)])
        i = _add(i, 1)
      end
    end
    parts = parts + [cur]
    return parts
  end
  def main()
    programText = _add(_add(_add(_add(_add(_add(_add(_add(_add(_add(_add(_add(_add(_add(_add(_add(_add(_add(_add(_add("Datasize: 1 Strings: 2\n", "\"count is: \"\n"), "\"\\n\"\n"), "    0 push  1\n"), "    5 store [0]\n"), "   10 fetch [0]\n"), "   15 push  10\n"), "   20 lt\n"), "   21 jz     (43) 65\n"), "   26 push  0\n"), "   31 prts\n"), "   32 fetch [0]\n"), "   37 prti\n"), "   38 push  1\n"), "   43 prts\n"), "   44 fetch [0]\n"), "   49 push  1\n"), "   54 add\n"), "   55 store [0]\n"), "   60 jmp    (-51) 10\n"), "   65 halt\n")
    prog = parseProgram(programText)
    runVM(prog)
  end
  main()
end_time = _now()
end_mem = _mem()
result = {"duration_us" => ((end_time - start) / 1000), "memory_bytes" => (end_mem - start_mem), "name" => "main"}
puts(JSON.pretty_generate(result))
