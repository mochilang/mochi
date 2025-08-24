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
  function slice_without_last($xs) {
  $res = [];
  $i = 0;
  while ($i < count($xs) - 1) {
  $res = _append($res, $xs[$i]);
  $i = $i + 1;
};
  return $res;
};
  function parse_float($token) {
  $sign = 1.0;
  $idx = 0;
  if (strlen($token) > 0) {
  $first = substr($token, 0, 1);
  if ($first == '-') {
  $sign = -1.0;
  $idx = 1;
} else {
  if ($first == '+') {
  $idx = 1;
};
};
}
  $int_part = 0;
  while ($idx < strlen($token) && substr($token, $idx, $idx + 1 - $idx) != '.') {
  $int_part = $int_part * 10 + intval(substr($token, $idx, $idx + 1 - $idx));
  $idx = $idx + 1;
};
  $result = 1.0 * $int_part;
  if ($idx < strlen($token) && substr($token, $idx, $idx + 1 - $idx) == '.') {
  $idx = $idx + 1;
  $place = 0.1;
  while ($idx < strlen($token)) {
  $digit = intval(substr($token, $idx, $idx + 1 - $idx));
  $result = $result + $place * (1.0 * $digit);
  $place = $place / 10.0;
  $idx = $idx + 1;
};
}
  return $sign * $result;
};
  function pow_float($base, $exp) {
  $result = 1.0;
  $i = 0;
  $e = intval($exp);
  while ($i < $e) {
  $result = $result * $base;
  $i = $i + 1;
};
  return $result;
};
  function apply_op($a, $b, $op) {
  if ($op == '+') {
  return $a + $b;
}
  if ($op == '-') {
  return $a - $b;
}
  if ($op == '*') {
  return $a * $b;
}
  if ($op == '/') {
  return $a / $b;
}
  if ($op == '^') {
  return pow_float($a, $b);
}
  return 0.0;
};
  function evaluate($tokens) {
  if (count($tokens) == 0) {
  return 0.0;
}
  $stack = [];
  foreach ($tokens as $token) {
  if ($token == '+' || $token == '-' || $token == '*' || $token == '/' || $token == '^') {
  if (($token == '+' || $token == '-') && count($stack) < 2) {
  $b = $stack[count($stack) - 1];
  $stack = slice_without_last($stack);
  if ($token == '-') {
  $stack = _append($stack, 0.0 - $b);
} else {
  $stack = _append($stack, $b);
};
} else {
  $b = $stack[count($stack) - 1];
  $stack = slice_without_last($stack);
  $a = $stack[count($stack) - 1];
  $stack = slice_without_last($stack);
  $result = apply_op($a, $b, $token);
  $stack = _append($stack, $result);
};
} else {
  $stack = _append($stack, parse_float($token));
}
};
  if (count($stack) != 1) {
  _panic('Invalid postfix expression');
}
  return $stack[0];
};
  echo rtrim(_str(evaluate(['2', '1', '+', '3', '*']))), PHP_EOL;
  echo rtrim(_str(evaluate(['4', '13', '5', '/', '+']))), PHP_EOL;
  echo rtrim(_str(evaluate(['5', '6', '9', '*', '+']))), PHP_EOL;
  echo rtrim(_str(evaluate(['2', '-', '3', '+']))), PHP_EOL;
  echo rtrim(_str(evaluate([]))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
