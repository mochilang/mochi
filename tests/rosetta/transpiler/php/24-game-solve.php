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
$OP_ADD = 1;
$OP_SUB = 2;
$OP_MUL = 3;
$OP_DIV = 4;
function binEval($op, $l, $r) {
  global $OP_ADD, $OP_SUB, $OP_MUL, $OP_DIV, $binString, $newNum, $exprEval, $exprString, $n_cards, $goal, $digit_range, $solve, $main;
  $lv = exprEval($l);
  $rv = exprEval($r);
  if ($op == $OP_ADD) {
  return ['num' => $lv['num'] * $rv['denom'] + $lv['denom'] * $rv['num'], 'denom' => $lv['denom'] * $rv['denom']];
}
  if ($op == $OP_SUB) {
  return ['num' => $lv['num'] * $rv['denom'] - $lv['denom'] * $rv['num'], 'denom' => $lv['denom'] * $rv['denom']];
}
  if ($op == $OP_MUL) {
  return ['num' => $lv['num'] * $rv['num'], 'denom' => $lv['denom'] * $rv['denom']];
}
  return ['num' => $lv['num'] * $rv['denom'], 'denom' => $lv['denom'] * $rv['num']];
}
function binString($op, $l, $r) {
  global $OP_ADD, $OP_SUB, $OP_MUL, $OP_DIV, $binEval, $newNum, $exprEval, $exprString, $n_cards, $goal, $digit_range, $solve, $main;
  $ls = exprString($l);
  $rs = exprString($r);
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
  return '(' . $ls . $opstr . $rs . ')';
}
function newNum($n) {
  global $OP_ADD, $OP_SUB, $OP_MUL, $OP_DIV, $binEval, $binString, $exprEval, $exprString, $n_cards, $goal, $digit_range, $solve, $main;
  return ['__tag' => 'Num', 'value' => ['num' => $n, 'denom' => 1]];
}
function exprEval($x) {
  global $OP_ADD, $OP_SUB, $OP_MUL, $OP_DIV, $binEval, $binString, $newNum, $exprString, $n_cards, $goal, $digit_range, $solve, $main;
  return (function($__v) {
  if ($__v['__tag'] === "Num") {
    $v = $__v["value"];
    return $v;
  } elseif ($__v['__tag'] === "Bin") {
    $op = $__v["op"];
    $l = $__v["left"];
    $r = $__v["right"];
    return binEval($op, $l, $r);
  }
})($x);
}
function exprString($x) {
  global $OP_ADD, $OP_SUB, $OP_MUL, $OP_DIV, $binEval, $binString, $newNum, $exprEval, $n_cards, $goal, $digit_range, $solve, $main;
  return (function($__v) {
  if ($__v['__tag'] === "Num") {
    $v = $__v["value"];
    return json_encode($v['num'], 1344);
  } elseif ($__v['__tag'] === "Bin") {
    $op = $__v["op"];
    $l = $__v["left"];
    $r = $__v["right"];
    return binString($op, $l, $r);
  }
})($x);
}
$n_cards = 4;
$goal = 24;
$digit_range = 9;
function solve($xs) {
  global $OP_ADD, $OP_SUB, $OP_MUL, $OP_DIV, $binEval, $binString, $newNum, $exprEval, $exprString, $n_cards, $goal, $digit_range, $main;
  if (count($xs) == 1) {
  $f = exprEval($xs[0]);
  if ($f['denom'] != 0 && $f['num'] == $f['denom'] * $goal) {
  echo exprString($xs[0]), PHP_EOL;
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
  foreach ([$OP_ADD, $OP_SUB, $OP_MUL, $OP_DIV] as $op) {
  $node = ['__tag' => 'Bin', 'op' => $op, 'left' => $a, 'right' => $b];
  if (solve(array_merge($rest, [$node]))) {
  return true;
}
};
  $node = ['__tag' => 'Bin', 'op' => $OP_SUB, 'left' => $b, 'right' => $a];
  if (solve(array_merge($rest, [$node]))) {
  return true;
}
  $node = ['__tag' => 'Bin', 'op' => $OP_DIV, 'left' => $b, 'right' => $a];
  if (solve(array_merge($rest, [$node]))) {
  return true;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return false;
}
function main() {
  global $OP_ADD, $OP_SUB, $OP_MUL, $OP_DIV, $binEval, $binString, $newNum, $exprEval, $exprString, $n_cards, $goal, $digit_range, $solve;
  $iter = 0;
  while ($iter < 10) {
  $cards = [];
  $i = 0;
  while ($i < $n_cards) {
  $n = (_now() % ($digit_range - 1)) + 1;
  $cards = array_merge($cards, [newNum($n)]);
  echo ' ' . json_encode($n, 1344), PHP_EOL;
  $i = $i + 1;
};
  echo ':  ', PHP_EOL;
  if (!solve($cards)) {
  echo 'No solution', PHP_EOL;
}
  $iter = $iter + 1;
};
}
main();
