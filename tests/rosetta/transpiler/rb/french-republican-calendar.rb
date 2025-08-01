# Generated by Mochi transpiler v0.10.54 on 2025-08-02 15:25 +0700
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
  def greLeap(year)
    a = ((year % 4)).to_i
    b = ((year % 100)).to_i
    c = ((year % 400)).to_i
    return a == 0 && (b != 0 || c == 0)
  end
  def repLeap(year)
    a = (((_add(year, 1)) % 4)).to_i
    b = (((_add(year, 1)) % 100)).to_i
    c = (((_add(year, 1)) % 400)).to_i
    return a == 0 && (b != 0 || c == 0)
  end
  def greToDay(d, m, y)
    yy = y
    mm = m
    if mm < 3
      yy = yy - 1
      mm = _add(mm, 12)
    end
    return _add(_add(_add(yy * 36525 / 100 - yy / 100, yy / 400), 306 * (_add(mm, 1)) / 10), d) - 654842
  end
  def repToDay(d, m, y)
    dd = d
    mm = m
    if mm == 13
      mm = mm - 1
      dd = _add(dd, 30)
    end
    if repLeap(y)
      dd = dd - 1
    end
    return _add(_add(_add(_add(365 * y, (_add(y, 1)) / 4) - (_add(y, 1)) / 100, (_add(y, 1)) / 400), 30 * mm), dd) - 395
  end
  def dayToGre(day)
    y = day * 100 / 36525
    d = _add(day - y * 36525 / 100, 21)
    y = _add(y, 1792)
    d = _add(d, y / 100) - y / 400 - 13
    m = 8
    while d > $gregorian[m]
      d = d - $gregorian[m]
      m = _add(m, 1)
      if m == 12
        m = 0
        y = _add(y, 1)
        if greLeap(y)
          $gregorian[1] = 29
        else
          $gregorian[1] = 28
        end
      end
    end
    m = _add(m, 1)
    return [d, m, y]
  end
  def dayToRep(day)
    y = (day - 1) * 100 / 36525
    if repLeap(y)
      y = y - 1
    end
    d = _add(_add(day - (_add(y, 1)) * 36525 / 100, 365), (_add(y, 1)) / 100) - (_add(y, 1)) / 400
    y = _add(y, 1)
    m = 1
    sc = 5
    if repLeap(y)
      sc = 6
    end
    while d > 30
      d = d - 30
      m = _add(m, 1)
      if m == 13
        if d > sc
          d = d - sc
          m = 1
          y = _add(y, 1)
          sc = 5
          if repLeap(y)
            sc = 6
          end
        end
      end
    end
    return [d, m, y]
  end
  def formatRep(d, m, y)
    if m == 13
      return _add(_add($sansculotidesStr[d - 1], " "), (y).to_s)
    end
    return _add(_add(_add(_add((d).to_s, " "), $republicanStr[m - 1]), " "), (y).to_s)
  end
  def formatGre(d, m, y)
    return _add(_add(_add(_add((d).to_s, " "), $gregorianStr[m - 1]), " "), (y).to_s)
  end
  $gregorianStr = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  $gregorian = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  $republicanStr = ["Vendemiaire", "Brumaire", "Frimaire", "Nivose", "Pluviose", "Ventose", "Germinal", "Floreal", "Prairial", "Messidor", "Thermidor", "Fructidor"]
  $sansculotidesStr = ["Fete de la vertu", "Fete du genie", "Fete du travail", "Fete de l'opinion", "Fete des recompenses", "Fete de la Revolution"]
  $rep = dayToRep(greToDay(20, 5, 1795))
  puts(formatRep($rep[0], $rep[1], $rep[2]))
  $gre = dayToGre(repToDay(1, 9, 3))
  puts(formatGre($gre[0], $gre[1], $gre[2]))
end_time = _now()
end_mem = _mem()
result = {"duration_us" => ((end_time - start) / 1000), "memory_bytes" => (end_mem - start_mem), "name" => "main"}
puts(JSON.pretty_generate(result))
