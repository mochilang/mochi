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
  function subarray($xs, $start, $end) {
  $result = [];
  $k = $start;
  while ($k < $end) {
  $result = _append($result, $xs[$k]);
  $k = _iadd($k, 1);
};
  return $result;
};
  function merge($arr) {
  if (count($arr) > 1) {
  $middle_length = _idiv(count($arr), 2);
  $left_array = subarray($arr, 0, $middle_length);
  $right_array = subarray($arr, $middle_length, count($arr));
  $left_size = count($left_array);
  $right_size = count($right_array);
  merge($left_array);
  merge($right_array);
  $left_index = 0;
  $right_index = 0;
  $index = 0;
  while ($left_index < $left_size && $right_index < $right_size) {
  if ($left_array[$left_index] < $right_array[$right_index]) {
  $arr[$index] = $left_array[$left_index];
  $left_index = _iadd($left_index, 1);
} else {
  $arr[$index] = $right_array[$right_index];
  $right_index = _iadd($right_index, 1);
}
  $index = _iadd($index, 1);
};
  while ($left_index < $left_size) {
  $arr[$index] = $left_array[$left_index];
  $left_index = _iadd($left_index, 1);
  $index = _iadd($index, 1);
};
  while ($right_index < $right_size) {
  $arr[$index] = $right_array[$right_index];
  $right_index = _iadd($right_index, 1);
  $index = _iadd($index, 1);
};
}
  return $arr;
};
  echo rtrim(_str(merge([10, 9, 8, 7, 6, 5, 4, 3, 2, 1]))), PHP_EOL;
  echo rtrim(_str(merge([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]))), PHP_EOL;
  echo rtrim(_str(merge([10, 22, 1, 2, 3, 9, 15, 23]))), PHP_EOL;
  echo rtrim(_str(merge([100]))), PHP_EOL;
  echo rtrim(_str(merge([]))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
