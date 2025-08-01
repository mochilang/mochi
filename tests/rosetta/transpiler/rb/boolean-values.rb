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
  def parseBool(s)
    l = s.downcase()
    if l == "1" || l == "t" || l == true || l == "yes" || l == "y"
      return true
    end
    return false
  end
  def main()
    n = true
    puts((n ? 'True' : 'False'))
    puts("bool")
    n = !n
    puts((n ? 'True' : 'False'))
    x = 5
    y = 8
    puts((["x == y:", (x == y ? 'True' : 'False')]).map{ |x| if x.nil? then 'None' elsif x == true then 'True' elsif x == false then 'False' elsif x.respond_to?(:to_h) then '{' + x.to_h.map{ |k,v| "'#{k}': #{v.is_a?(String) ? '\'' + v + '\'' : v.to_s}" }.join(', ') + '}' else x end }.join(' ').rstrip())
    puts((["x < y:", (x < y ? 'True' : 'False')]).map{ |x| if x.nil? then 'None' elsif x == true then 'True' elsif x == false then 'False' elsif x.respond_to?(:to_h) then '{' + x.to_h.map{ |k,v| "'#{k}': #{v.is_a?(String) ? '\'' + v + '\'' : v.to_s}" }.join(', ') + '}' else x end }.join(' ').rstrip())
    puts("\nConvert String into Boolean Data type\n")
    str1 = "japan"
    puts((["Before :", "string"]).map{ |x| if x.nil? then 'None' elsif x == true then 'True' elsif x == false then 'False' elsif x.respond_to?(:to_h) then '{' + x.to_h.map{ |k,v| "'#{k}': #{v.is_a?(String) ? '\'' + v + '\'' : v.to_s}" }.join(', ') + '}' else x end }.join(' ').rstrip())
    bolStr = parseBool(str1)
    puts((["After :", "bool"]).map{ |x| if x.nil? then 'None' elsif x == true then 'True' elsif x == false then 'False' elsif x.respond_to?(:to_h) then '{' + x.to_h.map{ |k,v| "'#{k}': #{v.is_a?(String) ? '\'' + v + '\'' : v.to_s}" }.join(', ') + '}' else x end }.join(' ').rstrip())
  end
  main()
end_time = _now()
end_mem = _mem()
result = {"duration_us" => ((end_time - start) / 1000), "memory_bytes" => (end_mem - start_mem), "name" => "main"}
puts(JSON.pretty_generate(result))
