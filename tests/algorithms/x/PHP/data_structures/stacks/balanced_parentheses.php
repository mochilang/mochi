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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function pop_last($xs) {
  global $idx, $tests;
  $res = [];
  $i = 0;
  while ($i < count($xs) - 1) {
  $res = _append($res, $xs[$i]);
  $i = $i + 1;
};
  return $res;
};
  function balanced_parentheses($s) {
  global $idx, $tests;
  $stack = [];
  $pairs = ['(' => ')', '[' => ']', '{' => '}'];
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if (array_key_exists($ch, $pairs)) {
  $stack = _append($stack, $ch);
} else {
  if ($ch == ')' || $ch == ']' || $ch == '}') {
  if (count($stack) == 0) {
  return false;
};
  $top = $stack[count($stack) - 1];
  if ($pairs[$top] != $ch) {
  return false;
};
  $stack = pop_last($stack);
};
}
  $i = $i + 1;
};
  return count($stack) == 0;
};
  $tests = ['([]{})', '[()]{}{[()()]()}', '[(])', '1+2*3-4', ''];
  $idx = 0;
  while ($idx < count($tests)) {
  echo rtrim(json_encode(balanced_parentheses($tests[$idx]), 1344)), PHP_EOL;
  $idx = $idx + 1;
}
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
