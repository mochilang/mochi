<?php
error_reporting(E_ALL & ~E_DEPRECATED);
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
function parseIntStr($s, $base = 10) {
    return intval($s, intval($base));
}
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function _slice($x, $start, $length = null) {
    if (is_string($x)) {
        if ($length === null) return substr($x, $start);
        return substr($x, $start, $length);
    }
    if ($length === null) return array_slice($x, $start);
    return array_slice($x, $start, $length);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function mochi_parseIntStr($str) {
  $digits = ['0' => 0, '1' => 1, '2' => 2, '3' => 3, '4' => 4, '5' => 5, '6' => 6, '7' => 7, '8' => 8, '9' => 9];
  $i = 0;
  $n = 0;
  while ($i < strlen($str)) {
  $n = $n * 10 + (intval($digits[$str[$i]]));
  $i = $i + 1;
};
  return $n;
};
  function precedence($op) {
  if ($op == '+' || $op == '-') {
  return 1;
}
  if ($op == '*' || $op == '/') {
  return 2;
}
  return 0;
};
  function parse($s) {
  $ops = [];
  $vals = [];
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch >= 'a' && $ch <= 'z') {
  $vals = _append($vals, ['kind' => 'var', 'val' => &$ch]);
} else {
  if ($ch == '(') {
  $ops = _append($ops, $ch);
} else {
  if ($ch == ')') {
  while (count($ops) > 0 && $ops[count($ops) - 1] != '(') {
  $op = $ops[count($ops) - 1];
  $ops = _slice($ops, 0, count($ops) - 1);
  $right = $vals[count($vals) - 1];
  $vals = _slice($vals, 0, count($vals) - 1);
  $left = $vals[count($vals) - 1];
  $vals = _slice($vals, 0, count($vals) - 1);
  $vals = _append($vals, ['kind' => &$op, 'op' => &$op, 'left' => &$left, 'right' => &$right]);
};
  $ops = _slice($ops, 0, count($ops) - 1);
} else {
  while (count($ops) > 0 && $ops[count($ops) - 1] != '(' && precedence($ops[count($ops) - 1]) >= precedence($ch)) {
  $op = $ops[count($ops) - 1];
  $ops = _slice($ops, 0, count($ops) - 1);
  $right = $vals[count($vals) - 1];
  $vals = _slice($vals, 0, count($vals) - 1);
  $left = $vals[count($vals) - 1];
  $vals = _slice($vals, 0, count($vals) - 1);
  $vals = _append($vals, ['kind' => &$op, 'op' => &$op, 'left' => &$left, 'right' => &$right]);
};
  $ops = _append($ops, $ch);
};
};
}
  $i = $i + 1;
};
  while (count($ops) > 0) {
  $op = $ops[count($ops) - 1];
  $ops = _slice($ops, 0, count($ops) - 1);
  $right = $vals[count($vals) - 1];
  $vals = _slice($vals, 0, count($vals) - 1);
  $left = $vals[count($vals) - 1];
  $vals = _slice($vals, 0, count($vals) - 1);
  $vals = _append($vals, ['kind' => &$op, 'op' => &$op, 'left' => &$left, 'right' => &$right]);
};
  return $vals[count($vals) - 1];
};
  function needParen($parent, $isRight, $child) {
  if ($child['kind'] != 'op') {
  return false;
}
  $p = precedence($parent);
  $c = precedence($child['op']);
  if ($c < $p) {
  return true;
}
  if ($c > $p) {
  return false;
}
  if ($parent == '-' && $isRight && ($child['op'] == '+' || $child['op'] == '-')) {
  return true;
}
  if ($parent == '/' && $isRight && ($child['op'] == '*' || $child['op'] == '/')) {
  return true;
}
  return false;
};
  function formatRec($node, $parent, $isRight) {
  if ($node['kind'] != 'op') {
  return $node['val'];
}
  $left = formatRec($node['left'], $node['op'], false);
  $right = formatRec($node['right'], $node['op'], true);
  $res = $left . $node['op'] . $right;
  if ($parent != '' && needParen($parent, $isRight, $node)) {
  $res = '(' . $res . ')';
}
  return $res;
};
  function makeNice($s) {
  $root = parse($s);
  return formatRec($root, '', false);
};
  function main() {
  $tStr = trim(fgets(STDIN));
  if ($tStr == '') {
  return;
}
  $t = parseIntStr($tStr, 10);
  for ($_ = 0; $_ < $t; $_++) {
  $line = trim(fgets(STDIN));
  echo rtrim(makeNice($line)), PHP_EOL;
};
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
