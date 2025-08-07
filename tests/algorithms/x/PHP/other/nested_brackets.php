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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  $OPEN_TO_CLOSED = ['(' => ')', '[' => ']', '{' => '}'];
  function slice_without_last($xs) {
  global $OPEN_TO_CLOSED;
  $res = [];
  $i = 0;
  while ($i < count($xs) - 1) {
  $res = _append($res, $xs[$i]);
  $i = $i + 1;
};
  return $res;
};
  function is_balanced($s) {
  global $OPEN_TO_CLOSED;
  $stack = [];
  $i = 0;
  while ($i < strlen($s)) {
  $symbol = substr($s, $i, $i + 1 - $i);
  if (array_key_exists($symbol, $OPEN_TO_CLOSED)) {
  $stack = _append($stack, $symbol);
} else {
  if ($symbol == ')' || $symbol == ']' || $symbol == '}') {
  if (count($stack) == 0) {
  return false;
};
  $top = $stack[count($stack) - 1];
  if ($OPEN_TO_CLOSED[$top] != $symbol) {
  return false;
};
  $stack = slice_without_last($stack);
};
}
  $i = $i + 1;
};
  return count($stack) == 0;
};
  function main() {
  global $OPEN_TO_CLOSED;
  echo rtrim(json_encode(is_balanced(''), 1344)), PHP_EOL;
  echo rtrim(json_encode(is_balanced('()'), 1344)), PHP_EOL;
  echo rtrim(json_encode(is_balanced('[]'), 1344)), PHP_EOL;
  echo rtrim(json_encode(is_balanced('{}'), 1344)), PHP_EOL;
  echo rtrim(json_encode(is_balanced('()[]{}'), 1344)), PHP_EOL;
  echo rtrim(json_encode(is_balanced('(())'), 1344)), PHP_EOL;
  echo rtrim(json_encode(is_balanced('[['), 1344)), PHP_EOL;
  echo rtrim(json_encode(is_balanced('([{}])'), 1344)), PHP_EOL;
  echo rtrim(json_encode(is_balanced('(()[)]'), 1344)), PHP_EOL;
  echo rtrim(json_encode(is_balanced('([)]'), 1344)), PHP_EOL;
  echo rtrim(json_encode(is_balanced('[[()]]'), 1344)), PHP_EOL;
  echo rtrim(json_encode(is_balanced('(()(()))'), 1344)), PHP_EOL;
  echo rtrim(json_encode(is_balanced(']'), 1344)), PHP_EOL;
  echo rtrim(json_encode(is_balanced('Life is a bowl of cherries.'), 1344)), PHP_EOL;
  echo rtrim(json_encode(is_balanced('Life is a bowl of che{}ies.'), 1344)), PHP_EOL;
  echo rtrim(json_encode(is_balanced('Life is a bowl of che}{ies.'), 1344)), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
