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
  function precedence($op) {
  if ($op == '+' || $op == '-') {
  return 1;
}
  if ($op == '*' || $op == '/') {
  return 2;
}
  if ($op == '^') {
  return 3;
}
  return 0;
};
  function popTop($stack) {
  return $stack[count($stack) - 1];
};
  function popStack($stack) {
  $newStack = [];
  $i = 0;
  while ($i < count($stack) - 1) {
  $newStack = _append($newStack, $stack[$i]);
  $i = $i + 1;
};
  return $newStack;
};
  function toRPN($expr) {
  $out = '';
  $stack = [];
  $i = 0;
  while ($i < strlen($expr)) {
  $ch = substr($expr, $i, $i + 1 - $i);
  if ($ch >= 'a' && $ch <= 'z') {
  $out = $out . $ch;
} else {
  if ($ch == '(') {
  $stack = _append($stack, $ch);
} else {
  if ($ch == ')') {
  while (count($stack) > 0) {
  $top = popTop($stack);
  if ($top == '(') {
  $stack = popStack($stack);
  break;
}
  $out = $out . $top;
  $stack = popStack($stack);
};
} else {
  $prec = precedence($ch);
  while (count($stack) > 0) {
  $top = popTop($stack);
  if ($top == '(') {
  break;
}
  $topPrec = precedence($top);
  if ($topPrec > $prec || ($topPrec == $prec && $ch != '^')) {
  $out = $out . $top;
  $stack = popStack($stack);
} else {
  break;
}
};
  $stack = _append($stack, $ch);
};
};
}
  $i = $i + 1;
};
  while (count($stack) > 0) {
  $top = popTop($stack);
  $out = $out . $top;
  $stack = popStack($stack);
};
  return $out;
};
  function main() {
  $t = intval(trim(fgets(STDIN)));
  $i = 0;
  while ($i < $t) {
  $expr = trim(fgets(STDIN));
  echo toRPN($expr), PHP_EOL;
  $i = $i + 1;
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
