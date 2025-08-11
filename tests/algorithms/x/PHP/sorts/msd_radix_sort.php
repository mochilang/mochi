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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function get_bit_length($n) {
  global $ex1, $sorted1, $ex2, $sorted2, $ex3, $sorted3, $ex4, $sorted4;
  if ($n == 0) {
  return 1;
}
  $length = 0;
  $num = $n;
  while ($num > 0) {
  $length = _iadd($length, 1);
  $num = _intdiv($num, 2);
};
  return $length;
};
  function max_bit_length($nums) {
  global $ex1, $sorted1, $ex2, $sorted2, $ex3, $sorted3, $ex4, $sorted4;
  $i = 0;
  $max_len = 0;
  while ($i < count($nums)) {
  $l = get_bit_length($nums[$i]);
  if ($l > $max_len) {
  $max_len = $l;
}
  $i = _iadd($i, 1);
};
  return $max_len;
};
  function get_bit($num, $pos) {
  global $ex1, $sorted1, $ex2, $sorted2, $ex3, $sorted3, $ex4, $sorted4;
  $n = $num;
  $i = 0;
  while ($i < $pos) {
  $n = _intdiv($n, 2);
  $i = _iadd($i, 1);
};
  return _imod($n, 2);
};
  function _msd_radix_sort($nums, $bit_position) {
  global $ex1, $sorted1, $ex2, $sorted2, $ex3, $sorted3, $ex4, $sorted4;
  if ($bit_position == 0 || count($nums) <= 1) {
  return $nums;
}
  $zeros = [];
  $ones = [];
  $i = 0;
  while ($i < count($nums)) {
  $num = $nums[$i];
  if (get_bit($num, _isub($bit_position, 1)) == 1) {
  $ones = _append($ones, $num);
} else {
  $zeros = _append($zeros, $num);
}
  $i = _iadd($i, 1);
};
  $zeros = _msd_radix_sort($zeros, _isub($bit_position, 1));
  $ones = _msd_radix_sort($ones, _isub($bit_position, 1));
  $res = $zeros;
  $i = 0;
  while ($i < count($ones)) {
  $res = _append($res, $ones[$i]);
  $i = _iadd($i, 1);
};
  return $res;
};
  function msd_radix_sort($nums) {
  global $ex1, $sorted1, $ex2, $sorted2, $ex3, $sorted3, $ex4, $sorted4;
  if (count($nums) == 0) {
  return [];
}
  $i = 0;
  while ($i < count($nums)) {
  if ($nums[$i] < 0) {
  _panic('All numbers must be positive');
}
  $i = _iadd($i, 1);
};
  $bits = max_bit_length($nums);
  $result = _msd_radix_sort($nums, $bits);
  return $result;
};
  function msd_radix_sort_inplace($nums) {
  global $ex1, $sorted1, $ex2, $sorted2, $ex3, $sorted3, $ex4, $sorted4;
  return msd_radix_sort($nums);
};
  $ex1 = [40, 12, 1, 100, 4];
  $sorted1 = msd_radix_sort($ex1);
  echo rtrim(_str($sorted1)), PHP_EOL;
  $ex2 = [];
  $sorted2 = msd_radix_sort($ex2);
  echo rtrim(_str($sorted2)), PHP_EOL;
  $ex3 = [123, 345, 123, 80];
  $sorted3 = msd_radix_sort($ex3);
  echo rtrim(_str($sorted3)), PHP_EOL;
  $ex4 = [1209, 834598, 1, 540402, 45];
  $sorted4 = msd_radix_sort($ex4);
  echo rtrim(_str($sorted4)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
