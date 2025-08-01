# Generated by Mochi transpiler v0.10.54 on 2025-08-02 14:11 +0700
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


def _indexOf(s, ch)
  idx = s.index(ch)
  idx ? idx : -1
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
  def indexOf(s, sub)
    i = 0
    while i <= s.length - sub.length
      if s[i..._add(i, sub.length)] == sub
        return i
      end
      i = _add(i, 1)
    end
    return -1
  end
  def removeChar(s, ch)
    res = ""
    i = 0
    while i < s.length
      c = s[i..._add(i, 1)]
      if c != ch
        res = _add(res, c)
      end
      i = _add(i, 1)
    end
    return res
  end
  def g2l(pieces)
    res = ""
    i = 0
    while i < pieces.length
      ch = pieces[i..._add(i, 1)]
      res = _add(res, $g2lMap[ch])
      i = _add(i, 1)
    end
    return res
  end
  def countChar(s, ch)
    c = 0
    i = 0
    while i < s.length
      if s[i..._add(i, 1)] == ch
        c = _add(c, 1)
      end
      i = _add(i, 1)
    end
    return c
  end
  def spid(pieces)
    pieces = g2l(pieces)
    if pieces.length != 8
      return -1
    end
    ["K", "Q"].each do |one|
      if countChar(pieces, one) != 1
        return -1
      end
    end
    ["R", "N", "B"].each do |two|
      if countChar(pieces, two) != 2
        return -1
      end
    end
    r1 = _indexOf(pieces, "R")
    r2 = _add(_add(_indexOf(pieces[_add(r1, 1)...pieces.length], "R"), r1), 1)
    k = _indexOf(pieces, "K")
    if k < r1 || k > r2
      return -1
    end
    b1 = _indexOf(pieces, "B")
    b2 = _add(_add(_indexOf(pieces[_add(b1, 1)...pieces.length], "B"), b1), 1)
    if (b2 - b1) % 2 == 0
      return -1
    end
    piecesN = removeChar(removeChar(pieces, "Q"), "B")
    n1 = _indexOf(piecesN, "N")
    n2 = _add(_add(_indexOf(piecesN[_add(n1, 1)...piecesN.length], "N"), n1), 1)
    _N = $ntable[_add((n1).to_s, (n2).to_s)]
    piecesQ = removeChar(pieces, "B")
    _Q = _indexOf(piecesQ, "Q")
    _D = _indexOf("0246", (b1).to_s)
    _L = _indexOf("1357", (b2).to_s)
    if _D == (0 - 1)
      _D = _indexOf("0246", (b2).to_s)
      _L = _indexOf("1357", (b1).to_s)
    end
    return _add(_add(_add(96 * _N, 16 * _Q), 4 * _D), _L)
  end
  def main()
    ["♕♘♖♗♗♘♔♖", "♖♘♗♕♔♗♘♖", "♖♕♘♗♗♔♖♘", "♖♘♕♗♗♔♖♘"].each do |pieces|
      puts(_add(_add(_add(_add(pieces, " or "), g2l(pieces)), " has SP-ID of "), (spid(pieces)).to_s))
    end
  end
  $g2lMap = {"♜" => "R", "♞" => "N", "♝" => "B", "♛" => "Q", "♚" => "K", "♖" => "R", "♘" => "N", "♗" => "B", "♕" => "Q", "♔" => "K"}
  $names = {"R" => "rook", "N" => "knight", "B" => "bishop", "Q" => "queen", "K" => "king"}
  $ntable = {"01" => 0, "02" => 1, "03" => 2, "04" => 3, "12" => 4, "13" => 5, "14" => 6, "23" => 7, "24" => 8, "34" => 9}
  main()
end_time = _now()
end_mem = _mem()
result = {"duration_us" => ((end_time - start) / 1000), "memory_bytes" => (end_mem - start_mem), "name" => "main"}
puts(JSON.pretty_generate(result))
