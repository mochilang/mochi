<?php
ini_set('memory_limit', '-1');
$now_seed = 0;
$now_seeded = false;
$s = getenv('MOCHI_NOW_SEED');
if ($s !== false && $s !== '') {
    $now_seed = intval($s);
    $now_seeded = true;
}
function _now() {
    global $now_seed, $now_seeded;
    if ($now_seeded) {
        $now_seed = ($now_seed * 1664525 + 1013904223) % 2147483647;
        return $now_seed;
    }
    return hrtime(true);
}
function _str($x) {
    if (is_array($x)) {
        $isList = array_keys($x) === range(0, count($x) - 1);
        if ($isList) {
            $parts = [];
            foreach ($x as $v) { $parts[] = _str($v); }
            return '[' . implode(' ', $parts) . ']';
        }
        $parts = [];
        foreach ($x as $k => $v) { $parts[] = _str($k) . ':' . _str($v); }
        return 'map[' . implode(' ', $parts) . ']';
    }
    if (is_bool($x)) return $x ? 'true' : 'false';
    if ($x === null) return 'null';
    return strval($x);
}
$__start_mem = memory_get_usage();
$__start = _now();
  $OP_ADD = 1;
  $OP_SUB = 2;
  $OP_MUL = 3;
  $OP_DIV = 4;
  function makeNode($n) {
  global $OP_ADD, $OP_SUB, $OP_MUL, $OP_DIV, $n_cards, $goal, $digit_range;
  return ['val' => ['num' => $n, 'denom' => 1], 'txt' => _str($n)];
};
  function combine($op, $l, $r) {
  global $OP_ADD, $OP_SUB, $OP_MUL, $OP_DIV, $n_cards, $goal, $digit_range;
  $res = null;
  if ($op == $OP_ADD) {
  $res = ['num' => $l['val']['num'] * $r['val']['denom'] + $l['val']['denom'] * $r['val']['num'], 'denom' => $l['val']['denom'] * $r['val']['denom']];
} else {
  if ($op == $OP_SUB) {
  $res = ['num' => $l['val']['num'] * $r['val']['denom'] - $l['val']['denom'] * $r['val']['num'], 'denom' => $l['val']['denom'] * $r['val']['denom']];
} else {
  if ($op == $OP_MUL) {
  $res = ['num' => $l['val']['num'] * $r['val']['num'], 'denom' => $l['val']['denom'] * $r['val']['denom']];
} else {
  $res = ['num' => $l['val']['num'] * $r['val']['denom'], 'denom' => $l['val']['denom'] * $r['val']['num']];
};
};
}
  $opstr = '';
  if ($op == $OP_ADD) {
  $opstr = ' + ';
} else {
  if ($op == $OP_SUB) {
  $opstr = ' - ';
} else {
  if ($op == $OP_MUL) {
  $opstr = ' * ';
} else {
  $opstr = ' / ';
};
};
}
  return ['val' => $res, 'txt' => '(' . $l['txt'] . $opstr . $r['txt'] . ')'];
};
  function exprEval($x) {
  global $OP_ADD, $OP_SUB, $OP_MUL, $OP_DIV, $n_cards, $goal, $digit_range;
  return $x['val'];
};
  function exprString($x) {
  global $OP_ADD, $OP_SUB, $OP_MUL, $OP_DIV, $n_cards, $goal, $digit_range;
  return $x['txt'];
};
  $n_cards = 4;
  $goal = 24;
  $digit_range = 9;
  function solve($xs) {
  global $OP_ADD, $OP_SUB, $OP_MUL, $OP_DIV, $n_cards, $goal, $digit_range;
  if (count($xs) == 1) {
  $f = exprEval($xs[0]);
  if ($f['denom'] != 0 && $f['num'] == $f['denom'] * $goal) {
  echo rtrim(exprString($xs[0])), PHP_EOL;
  return true;
};
  return false;
}
  $i = 0;
  while ($i < count($xs)) {
  $j = $i + 1;
  while ($j < count($xs)) {
  $rest = [];
  $k = 0;
  while ($k < count($xs)) {
  if ($k != $i && $k != $j) {
  $rest = array_merge($rest, [$xs[$k]]);
}
  $k = $k + 1;
};
  $a = $xs[$i];
  $b = $xs[$j];
  $node = null;
  foreach ([$OP_ADD, $OP_SUB, $OP_MUL, $OP_DIV] as $op) {
  $node = combine($op, $a, $b);
  if (solve(array_merge($rest, [$node]))) {
  return true;
}
};
  $node = combine($OP_SUB, $b, $a);
  if (solve(array_merge($rest, [$node]))) {
  return true;
}
  $node = combine($OP_DIV, $b, $a);
  if (solve(array_merge($rest, [$node]))) {
  return true;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return false;
};
  function main() {
  global $OP_ADD, $OP_SUB, $OP_MUL, $OP_DIV, $n_cards, $goal, $digit_range;
  $iter = 0;
  while ($iter < 10) {
  $cards = [];
  $i = 0;
  while ($i < $n_cards) {
  $n = (fmod(_now(), ($digit_range - 1))) + 1;
  $cards = array_merge($cards, [makeNode($n)]);
  echo rtrim(' ' . _str($n)), PHP_EOL;
  $i = $i + 1;
};
  echo rtrim(':  '), PHP_EOL;
  if (!solve($cards)) {
  echo rtrim('No solution'), PHP_EOL;
}
  $iter = $iter + 1;
};
};
  main();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
