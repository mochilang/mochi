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
  function cycle_sort(&$arr) {
  $n = count($arr);
  $cycle_start = 0;
  while ($cycle_start < _isub($n, 1)) {
  $item = $arr[$cycle_start];
  $pos = $cycle_start;
  $i = _iadd($cycle_start, 1);
  while ($i < $n) {
  if ($arr[$i] < $item) {
  $pos = _iadd($pos, 1);
}
  $i = _iadd($i, 1);
};
  if ($pos == $cycle_start) {
  $cycle_start = _iadd($cycle_start, 1);
  continue;
}
  while ($item == $arr[$pos]) {
  $pos = _iadd($pos, 1);
};
  $temp = $arr[$pos];
  $arr[$pos] = $item;
  $item = $temp;
  while ($pos != $cycle_start) {
  $pos = $cycle_start;
  $i = _iadd($cycle_start, 1);
  while ($i < $n) {
  if ($arr[$i] < $item) {
  $pos = _iadd($pos, 1);
}
  $i = _iadd($i, 1);
};
  while ($item == $arr[$pos]) {
  $pos = _iadd($pos, 1);
};
  $temp2 = $arr[$pos];
  $arr[$pos] = $item;
  $item = $temp2;
};
  $cycle_start = _iadd($cycle_start, 1);
};
  return $arr;
};
  echo rtrim(_str(cycle_sort([4, 3, 2, 1]))), PHP_EOL;
  echo rtrim(_str(cycle_sort([-4, 20, 0, -50, 100, -1]))), PHP_EOL;
  echo rtrim(_str(cycle_sort([]))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
