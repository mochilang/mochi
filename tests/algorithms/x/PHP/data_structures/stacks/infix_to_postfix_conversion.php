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
  $PRECEDENCES = ['*' => 2, '+' => 1, '-' => 1, '/' => 2, '^' => 3];
  $ASSOCIATIVITIES = ['*' => 'LR', '+' => 'LR', '-' => 'LR', '/' => 'LR', '^' => 'RL'];
  function precedence($ch) {
  global $ASSOCIATIVITIES, $PRECEDENCES;
  if (array_key_exists($ch, $PRECEDENCES)) {
  return $PRECEDENCES[$ch];
}
  return -1;
};
  function associativity($ch) {
  global $ASSOCIATIVITIES, $PRECEDENCES;
  if (array_key_exists($ch, $ASSOCIATIVITIES)) {
  return $ASSOCIATIVITIES[$ch];
}
  return '';
};
  function balanced_parentheses($expr) {
  global $ASSOCIATIVITIES, $PRECEDENCES;
  $count = 0;
  $i = 0;
  while ($i < strlen($expr)) {
  $ch = substr($expr, $i, $i + 1 - $i);
  if ($ch == '(') {
  $count = $count + 1;
}
  if ($ch == ')') {
  $count = $count - 1;
  if ($count < 0) {
  return false;
};
}
  $i = $i + 1;
};
  return $count == 0;
};
  function is_letter($ch) {
  global $ASSOCIATIVITIES, $PRECEDENCES;
  return ('a' <= $ch && $ch <= 'z') || ('A' <= $ch && $ch <= 'Z');
};
  function is_digit($ch) {
  global $ASSOCIATIVITIES, $PRECEDENCES;
  return '0' <= $ch && $ch <= '9';
};
  function is_alnum($ch) {
  global $ASSOCIATIVITIES, $PRECEDENCES;
  return is_letter($ch) || is_digit($ch);
};
  function infix_to_postfix($expression) {
  global $ASSOCIATIVITIES, $PRECEDENCES;
  if (balanced_parentheses($expression) == false) {
  _panic('Mismatched parentheses');
}
  $stack = [];
  $postfix = [];
  $i = 0;
  while ($i < strlen($expression)) {
  $ch = substr($expression, $i, $i + 1 - $i);
  if (is_alnum($ch)) {
  $postfix = _append($postfix, $ch);
} else {
  if ($ch == '(') {
  $stack = _append($stack, $ch);
} else {
  if ($ch == ')') {
  while (count($stack) > 0 && $stack[count($stack) - 1] != '(') {
  $postfix = _append($postfix, $stack[count($stack) - 1]);
  $stack = array_slice($stack, 0, count($stack) - 1);
};
  if (count($stack) > 0) {
  $stack = array_slice($stack, 0, count($stack) - 1);
};
} else {
  if ($ch == ' ') {
} else {
  while (true) {
  if (count($stack) == 0) {
  $stack = _append($stack, $ch);
  break;
}
  $cp = precedence($ch);
  $tp = precedence($stack[count($stack) - 1]);
  if ($cp > $tp) {
  $stack = _append($stack, $ch);
  break;
}
  if ($cp < $tp) {
  $postfix = _append($postfix, $stack[count($stack) - 1]);
  $stack = array_slice($stack, 0, count($stack) - 1);
  continue;
}
  if (associativity($ch) == 'RL') {
  $stack = _append($stack, $ch);
  break;
}
  $postfix = _append($postfix, $stack[count($stack) - 1]);
  $stack = array_slice($stack, 0, count($stack) - 1);
};
};
};
};
}
  $i = $i + 1;
};
  while (count($stack) > 0) {
  $postfix = _append($postfix, $stack[count($stack) - 1]);
  $stack = array_slice($stack, 0, count($stack) - 1);
};
  $res = '';
  $j = 0;
  while ($j < count($postfix)) {
  if ($j > 0) {
  $res = $res . ' ';
}
  $res = $res . $postfix[$j];
  $j = $j + 1;
};
  return $res;
};
  function main() {
  global $ASSOCIATIVITIES, $PRECEDENCES;
  $expression = 'a+b*(c^d-e)^(f+g*h)-i';
  echo rtrim($expression), PHP_EOL;
  echo rtrim(infix_to_postfix($expression)), PHP_EOL;
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
