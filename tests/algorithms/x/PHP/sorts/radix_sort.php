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
  $RADIX = 10;
  function make_buckets() {
  global $RADIX;
  $buckets = [];
  $i = 0;
  while ($i < $RADIX) {
  $buckets = _append($buckets, []);
  $i = _iadd($i, 1);
};
  return $buckets;
};
  function max_value($xs) {
  global $RADIX;
  $max_val = $xs[0];
  $i = 1;
  while ($i < count($xs)) {
  if ($xs[$i] > $max_val) {
  $max_val = $xs[$i];
}
  $i = _iadd($i, 1);
};
  return $max_val;
};
  function radix_sort(&$list_of_ints) {
  global $RADIX;
  $placement = 1;
  $max_digit = max_value($list_of_ints);
  while ($placement <= $max_digit) {
  $buckets = make_buckets();
  $i = 0;
  while ($i < count($list_of_ints)) {
  $value = $list_of_ints[$i];
  $tmp = _imod((_intdiv($value, $placement)), $RADIX);
  $buckets[$tmp] = _append($buckets[$tmp], $value);
  $i = _iadd($i, 1);
};
  $a = 0;
  $b = 0;
  while ($b < $RADIX) {
  $bucket = $buckets[$b];
  $j = 0;
  while ($j < count($bucket)) {
  $list_of_ints[$a] = $bucket[$j];
  $a = _iadd($a, 1);
  $j = _iadd($j, 1);
};
  $b = _iadd($b, 1);
};
  $placement = _imul($placement, $RADIX);
};
  return $list_of_ints;
};
  echo rtrim(_str(radix_sort([0, 5, 3, 2, 2]))), PHP_EOL;
  echo rtrim(_str(radix_sort([1, 100, 10, 1000]))), PHP_EOL;
  echo rtrim(_str(radix_sort([15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0]))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
