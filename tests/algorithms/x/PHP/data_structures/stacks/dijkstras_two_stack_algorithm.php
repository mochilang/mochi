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
function _intdiv($a, $b) {
    if ($b === 0 || $b === '0') {
        throw new DivisionByZeroError();
    }
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        $q = bcdiv($sa, $sb, 0);
        $rem = bcmod($sa, $sb);
        $neg = ((strpos($sa, '-') === 0) xor (strpos($sb, '-') === 0));
        if ($neg && bccomp($rem, '0') != 0) {
            $q = bcsub($q, '1');
        }
        return intval($q);
    }
    $ai = intval($a);
    $bi = intval($b);
    $q = intdiv($ai, $bi);
    if ((($ai ^ $bi) < 0) && ($ai % $bi != 0)) {
        $q -= 1;
    }
    return $q;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function is_digit($ch) {
  global $equation;
  return $ch == '0' || $ch == '1' || $ch == '2' || $ch == '3' || $ch == '4' || $ch == '5' || $ch == '6' || $ch == '7' || $ch == '8' || $ch == '9';
};
  function slice_without_last_int($xs) {
  global $equation;
  $res = [];
  $i = 0;
  while ($i < count($xs) - 1) {
  $res = _append($res, $xs[$i]);
  $i = $i + 1;
};
  return $res;
};
  function slice_without_last_string($xs) {
  global $equation;
  $res = [];
  $i = 0;
  while ($i < count($xs) - 1) {
  $res = _append($res, $xs[$i]);
  $i = $i + 1;
};
  return $res;
};
  function dijkstras_two_stack_algorithm($equation) {
  $operand_stack = [];
  $operator_stack = [];
  $idx = 0;
  while ($idx < strlen($equation)) {
  $ch = substr($equation, $idx, $idx + 1 - $idx);
  if (is_digit($ch)) {
  $operand_stack = _append($operand_stack, intval($ch));
} else {
  if ($ch == '+' || $ch == '-' || $ch == '*' || $ch == '/') {
  $operator_stack = _append($operator_stack, $ch);
} else {
  if ($ch == ')') {
  $opr = $operator_stack[count($operator_stack) - 1];
  $operator_stack = slice_without_last_string($operator_stack);
  $num1 = $operand_stack[count($operand_stack) - 1];
  $operand_stack = slice_without_last_int($operand_stack);
  $num2 = $operand_stack[count($operand_stack) - 1];
  $operand_stack = slice_without_last_int($operand_stack);
  $total = ($opr == '+' ? $num2 + $num1 : ($opr == '-' ? $num2 - $num1 : ($opr == '*' ? $num2 * $num1 : _intdiv($num2, $num1))));
  $operand_stack = _append($operand_stack, $total);
};
};
}
  $idx = $idx + 1;
};
  return $operand_stack[count($operand_stack) - 1];
};
  $equation = '(5 + ((4 * 2) * (2 + 3)))';
  echo rtrim($equation . ' = ' . _str(dijkstras_two_stack_algorithm($equation))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
