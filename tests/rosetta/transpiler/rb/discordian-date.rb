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
  def isLeap(y)
    if y % 400 == 0
      return true
    end
    if y % 100 == 0
      return false
    end
    return y % 4 == 0
  end
  def dayOfYear(y, m, d)
    doy = _add($daysBefore[m - 1], d)
    if m > 2 && isLeap(y)
      doy = _add(doy, 1)
    end
    return doy
  end
  def ordinal(n)
    suff = "th"
    mod100 = n % 100
    if mod100 < 11 || mod100 > 13
      r = n % 10
      if r == 1
        suff = "st"
      else
        if r == 2
          suff = "nd"
        else
          if r == 3
            suff = "rd"
          end
        end
      end
    end
    return _add((n).to_s, suff)
  end
  def discordian(y, m, d)
    if isLeap(y) && m == 2 && d == 29
      return _add("St. Tib's Day, YOLD ", (_add(y, 1166)).to_s)
    end
    doy = dayOfYear(y, m, d)
    if isLeap(y) && doy > 60
      doy = doy - 1
    end
    idx = doy - 1
    season = idx / 73
    day = idx % 73
    res = _add(_add(_add(_add(_add(_add($dayNames[idx % 5], ", the "), ordinal(_add(day, 1))), " day of "), $seasons[season]), " in the YOLD "), (_add(y, 1166)).to_s)
    if day == 4
      res = _add(_add(_add(res, ". Celebrate "), $holydays[season][0]), "!")
    end
    if day == 49
      res = _add(_add(_add(res, ". Celebrate "), $holydays[season][1]), "!")
    end
    return res
  end
  def main()
    dates = [[2010, 7, 22], [2012, 2, 28], [2012, 2, 29], [2012, 3, 1], [2012, 12, 31], [2013, 1, 1], [2100, 12, 31], [2015, 10, 19], [2010, 1, 5], [2011, 5, 3], [2000, 3, 13]]
    i = 0
    while i < dates.length
      dt = dates[i]
      puts(discordian(dt[0], dt[1], dt[2]))
      i = _add(i, 1)
    end
  end
  $dayNames = ["Sweetmorn", "Boomtime", "Pungenday", "Prickle-Prickle", "Setting Orange"]
  $seasons = ["Chaos", "Discord", "Confusion", "Bureaucracy", "The Aftermath"]
  $holydays = [["Mungday", "Chaoflux"], ["Mojoday", "Discoflux"], ["Syaday", "Confuflux"], ["Zaraday", "Bureflux"], ["Maladay", "Afflux"]]
  $daysBefore = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334]
  main()
end_time = _now()
end_mem = _mem()
result = {"duration_us" => ((end_time - start) / 1000), "memory_bytes" => (end_mem - start_mem), "name" => "main"}
puts(JSON.pretty_generate(result))
