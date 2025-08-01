# Generated by Mochi transpiler v0.10.50 on 2025-07-30 23:34 +0700

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

def repeat(ch, n)
  s = ""
  i = 0
  while i < n
    s = _add(s, ch)
    i = _add(i, 1)
  end
  return s
end
def chance(prob)
  threshold = (prob * 1000.0).to_i
  return _now() % 1000 < threshold
end
def newBoard()
  b = []
  r = 0
  while r < $rows
    row = []
    c = 0
    while c < $cols
      if _now() % 2 == 0
        row = row + ["T"]
      else
        row = row + [" "]
      end
      c = _add(c, 1)
    end
    b = b + [row]
    r = _add(r, 1)
  end
  return b
end
def step(src)
  dst = []
  r = 0
  while r < $rows
    row = []
    c = 0
    while c < $cols
      cell = src[r][c]
      next_ = cell
      if cell == "#"
        next_ = " "
      else
        if cell == "T"
          burning = false
          dr = -1
          while dr <= 1
            dc = -1
            while dc <= 1
              if dr != 0 || dc != 0
                rr = _add(r, dr)
                cc = _add(c, dc)
                if rr >= 0 && rr < $rows && cc >= 0 && cc < $cols
                  if src[rr][cc] == "#"
                    burning = true
                  end
                end
              end
              dc = _add(dc, 1)
            end
            dr = _add(dr, 1)
          end
          if burning || chance($f)
            next_ = "#"
          end
        else
          if chance($p)
            next_ = "T"
          end
        end
      end
      row = row + [next_]
      c = _add(c, 1)
    end
    dst = dst + [row]
    r = _add(r, 1)
  end
  return dst
end
def printBoard(b)
  puts(_add(_repeat("__", $cols), "\n\n"))
  r = 0
  while r < $rows
    line = ""
    c = 0
    while c < $cols
      cell = b[r][c]
      if cell == " "
        line = _add(line, "  ")
      else
        line = _add(_add(line, " "), cell)
      end
      c = _add(c, 1)
    end
    puts(_add(line, "\n"))
    r = _add(r, 1)
  end
end
$rows = 20
$cols = 30
$p = 0.01
$f = 0.001
$board = newBoard()
printBoard($board)
$board = step($board)
printBoard($board)
