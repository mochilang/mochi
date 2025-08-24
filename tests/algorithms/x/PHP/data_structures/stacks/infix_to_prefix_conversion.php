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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  $PRIORITY = ['%' => 2, '*' => 2, '+' => 1, '-' => 1, '/' => 2, '^' => 3];
  $LETTERS = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $DIGITS = '0123456789';
  function is_alpha($ch) {
  global $DIGITS, $LETTERS, $PRIORITY;
  $i = 0;
  while ($i < strlen($LETTERS)) {
  if (substr($LETTERS, $i, $i + 1 - $i) == $ch) {
  return true;
}
  $i = $i + 1;
};
  return false;
};
  function is_digit($ch) {
  global $DIGITS, $LETTERS, $PRIORITY;
  $i = 0;
  while ($i < strlen($DIGITS)) {
  if (substr($DIGITS, $i, $i + 1 - $i) == $ch) {
  return true;
}
  $i = $i + 1;
};
  return false;
};
  function reverse_string($s) {
  global $DIGITS, $LETTERS, $PRIORITY;
  $out = '';
  $i = strlen($s) - 1;
  while ($i >= 0) {
  $out = $out . substr($s, $i, $i + 1 - $i);
  $i = $i - 1;
};
  return $out;
};
  function infix_to_postfix($infix) {
  global $DIGITS, $LETTERS, $PRIORITY;
  $stack = [];
  $post = [];
  $i = 0;
  while ($i < strlen($infix)) {
  $x = substr($infix, $i, $i + 1 - $i);
  if (is_alpha($x) || is_digit($x)) {
  $post = _append($post, $x);
} else {
  if ($x == '(') {
  $stack = _append($stack, $x);
} else {
  if ($x == ')') {
  if (count($stack) == 0) {
  _panic('list index out of range');
};
  while ($stack[count($stack) - 1] != '(') {
  $post = _append($post, $stack[count($stack) - 1]);
  $stack = array_slice($stack, 0, count($stack) - 1);
};
  $stack = array_slice($stack, 0, count($stack) - 1);
} else {
  if (count($stack) == 0) {
  $stack = _append($stack, $x);
} else {
  while (count($stack) > 0 && $stack[count($stack) - 1] != '(' && $PRIORITY[$x] <= $PRIORITY[$stack[count($stack) - 1]]) {
  $post = _append($post, $stack[count($stack) - 1]);
  $stack = array_slice($stack, 0, count($stack) - 1);
};
  $stack = _append($stack, $x);
};
};
};
}
  $i = $i + 1;
};
  while (count($stack) > 0) {
  if ($stack[count($stack) - 1] == '(') {
  _panic('invalid expression');
}
  $post = _append($post, $stack[count($stack) - 1]);
  $stack = array_slice($stack, 0, count($stack) - 1);
};
  $res = '';
  $j = 0;
  while ($j < count($post)) {
  $res = $res . $post[$j];
  $j = $j + 1;
};
  return $res;
};
  function infix_to_prefix($infix) {
  global $DIGITS, $LETTERS, $PRIORITY;
  $reversed = '';
  $i = strlen($infix) - 1;
  while ($i >= 0) {
  $ch = substr($infix, $i, $i + 1 - $i);
  if ($ch == '(') {
  $reversed = $reversed . ')';
} else {
  if ($ch == ')') {
  $reversed = $reversed . '(';
} else {
  $reversed = $reversed . $ch;
};
}
  $i = $i - 1;
};
  $postfix = infix_to_postfix($reversed);
  $prefix = reverse_string($postfix);
  return $prefix;
};
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
