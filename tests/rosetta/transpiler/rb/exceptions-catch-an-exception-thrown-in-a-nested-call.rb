# Generated by Mochi transpiler v0.10.50 on 2025-07-31 00:15 +0700
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
  def baz()
    $bazCall = _add($bazCall, 1)
    puts("baz: start")
    if $bazCall == 1
      puts("baz: raising U0")
      return "U0"
    end
    if $bazCall == 2
      puts("baz: raising U1")
      return "U1"
    end
    puts("baz: end")
    return ""
  end
  def bar()
    puts("bar: start")
    err = baz()
    if err.length > 0
      return err
    end
    puts("bar: end")
    return ""
  end
  def foo()
    puts("foo: start")
    err = bar()
    if err == "U0"
      puts("foo: caught U0")
    else
      if err.length > 0
        return err
      end
    end
    err = bar()
    if err == "U0"
      puts("foo: caught U0")
    else
      if err.length > 0
        return err
      end
    end
    puts("foo: end")
    return ""
  end
  def main()
    puts("main: start")
    err = foo()
    if err.length > 0
      puts(_add("main: unhandled ", err))
    else
      puts("main: success")
    end
  end
  $bazCall = 0
  main()
end_time = _now()
end_mem = _mem()
result = {"duration_us" => ((end_time - start) / 1000), "memory_bytes" => (end_mem - start_mem), "name" => "main"}
puts(JSON.pretty_generate(result))
