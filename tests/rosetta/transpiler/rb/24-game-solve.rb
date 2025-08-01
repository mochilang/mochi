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
  def makeNode(n)
    return Node.new(val: Rational_.new(num: n, denom: 1), txt: (n).to_s)
  end
  def combine(op, l, r)
    res = {}
    if op == $OP_ADD
      res = Rational_.new(num: _add(l.val.num * r.val.denom, l.val.denom * r.val.num), denom: l.val.denom * r.val.denom)
    else
      if op == $OP_SUB
        res = Rational_.new(num: l.val.num * r.val.denom - l.val.denom * r.val.num, denom: l.val.denom * r.val.denom)
      else
        if op == $OP_MUL
          res = Rational_.new(num: l.val.num * r.val.num, denom: l.val.denom * r.val.denom)
        else
          res = Rational_.new(num: l.val.num * r.val.denom, denom: l.val.denom * r.val.num)
        end
      end
    end
    opstr = ""
    if op == $OP_ADD
      opstr = " + "
    else
      if op == $OP_SUB
        opstr = " - "
      else
        if op == $OP_MUL
          opstr = " * "
        else
          opstr = " / "
        end
      end
    end
    return Node.new(val: res, txt: _add(_add(_add(_add("(", l.txt), opstr), r.txt), ")"))
  end
  def exprEval(x)
    return x.val
  end
  def exprString(x)
    return x.txt
  end
  def solve(xs)
    if xs.length == 1
      f = exprEval(xs[0])
      if f.denom != 0 && f.num == f.denom * $goal
        puts(exprString(xs[0]))
        return true
      end
      return false
    end
    i = 0
    while i < xs.length
      j = _add(i, 1)
      while j < xs.length
        rest = []
        k = 0
        while k < xs.length
          if k != i && k != j
            rest = rest + [xs[k]]
          end
          k = _add(k, 1)
        end
        a = xs[i]
        b = xs[j]
        node = {}
        [$OP_ADD, $OP_SUB, $OP_MUL, $OP_DIV].each do |op|
          node = combine(op, a, b)
          if solve(rest + [node])
            return true
          end
        end
        node = combine($OP_SUB, b, a)
        if solve(rest + [node])
          return true
        end
        node = combine($OP_DIV, b, a)
        if solve(rest + [node])
          return true
        end
        j = _add(j, 1)
      end
      i = _add(i, 1)
    end
    return false
  end
  def main()
    iter = 0
    while iter < 10
      cards = []
      i = 0
      while i < $n_cards
        n = _add((_now() % ($digit_range - 1)), 1)
        cards = cards + [makeNode(n)]
        puts(_add(" ", (n).to_s))
        i = _add(i, 1)
      end
      puts(":  ")
      if !solve(cards)
        puts("No solution")
      end
      iter = _add(iter, 1)
    end
  end
  Rational_ = Struct.new(:num, :denom, keyword_init: true)
  $OP_ADD = 1
  $OP_SUB = 2
  $OP_MUL = 3
  $OP_DIV = 4
  Node = Struct.new(:val, :txt, keyword_init: true)
  $n_cards = 4
  $goal = 24
  $digit_range = 9
  main()
end_time = _now()
end_mem = _mem()
result = {"duration_us" => ((end_time - start) / 1000), "memory_bytes" => (end_mem - start_mem), "name" => "main"}
puts(JSON.pretty_generate(result))
