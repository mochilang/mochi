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
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
function _iadd($a, $b) {
    if (function_exists('bcadd')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcadd($sa, $sb, 0);
    }
    return $a + $b;
}
function _isub($a, $b) {
    if (function_exists('bcsub')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcsub($sa, $sb, 0);
    }
    return $a - $b;
}
function _imul($a, $b) {
    if (function_exists('bcmul')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcmul($sa, $sb, 0);
    }
    return $a * $b;
}
function _idiv($a, $b) {
    return _intdiv($a, $b);
}
function _imod($a, $b) {
    if (function_exists('bcmod')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcmod($sa, $sb));
    }
    return $a % $b;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function get_totients($max_one) {
  $totients = [];
  $i = 0;
  while ($i < $max_one) {
  $totients = _append($totients, $i);
  $i = _iadd($i, 1);
};
  $i = 2;
  while ($i < $max_one) {
  if ($totients[$i] == $i) {
  $x = $i;
  while ($x < $max_one) {
  $totients[$x] = _isub($totients[$x], _idiv($totients[$x], $i));
  $x = _iadd($x, $i);
};
}
  $i = _iadd($i, 1);
};
  return $totients;
};
  function has_same_digits($num1, $num2) {
  $count1 = [];
  $count2 = [];
  $i = 0;
  while ($i < 10) {
  $count1 = _append($count1, 0);
  $count2 = _append($count2, 0);
  $i = _iadd($i, 1);
};
  $n1 = $num1;
  $n2 = $num2;
  if ($n1 == 0) {
  $count1[0] = _iadd($count1[0], 1);
}
  if ($n2 == 0) {
  $count2[0] = _iadd($count2[0], 1);
}
  while ($n1 > 0) {
  $d1 = _imod($n1, 10);
  $count1[$d1] = _iadd($count1[$d1], 1);
  $n1 = _intdiv($n1, 10);
};
  while ($n2 > 0) {
  $d2 = _imod($n2, 10);
  $count2[$d2] = _iadd($count2[$d2], 1);
  $n2 = _intdiv($n2, 10);
};
  $i = 0;
  while ($i < 10) {
  if ($count1[$i] != $count2[$i]) {
  return false;
}
  $i = _iadd($i, 1);
};
  return true;
};
  function solution($max_n) {
  $min_numerator = 1;
  $min_denominator = 0;
  $totients = get_totients(_iadd($max_n, 1));
  $i = 2;
  while ($i <= $max_n) {
  $t = $totients[$i];
  if (_imul($i, $min_denominator) < _imul($min_numerator, $t) && has_same_digits($i, $t)) {
  $min_numerator = $i;
  $min_denominator = $t;
}
  $i = _iadd($i, 1);
};
  return $min_numerator;
};
  echo rtrim(_str(solution(10000))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
