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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function insert_next($collection, $index) {
  $arr = $collection;
  if ($index >= count($arr) || $arr[_isub($index, 1)] <= $arr[$index]) {
  return $arr;
}
  $j = _isub($index, 1);
  $temp = $arr[$j];
  $arr[$j] = $arr[$index];
  $arr[$index] = $temp;
  return insert_next($arr, _iadd($index, 1));
};
  function rec_insertion_sort($collection, $n) {
  $arr = $collection;
  if (count($arr) <= 1 || $n <= 1) {
  return $arr;
}
  $arr = insert_next($arr, _isub($n, 1));
  return rec_insertion_sort($arr, _isub($n, 1));
};
  function test_rec_insertion_sort() {
  $col1 = [1, 2, 1];
  $col1 = rec_insertion_sort($col1, count($col1));
  if ($col1[0] != 1 || $col1[1] != 1 || $col1[2] != 2) {
  _panic('test1 failed');
}
  $col2 = [2, 1, 0, -1, -2];
  $col2 = rec_insertion_sort($col2, count($col2));
  if ($col2[0] != (_isub(0, 2))) {
  _panic('test2 failed');
}
  if ($col2[1] != (_isub(0, 1))) {
  _panic('test2 failed');
}
  if ($col2[2] != 0) {
  _panic('test2 failed');
}
  if ($col2[3] != 1) {
  _panic('test2 failed');
}
  if ($col2[4] != 2) {
  _panic('test2 failed');
}
  $col3 = [1];
  $col3 = rec_insertion_sort($col3, count($col3));
  if ($col3[0] != 1) {
  _panic('test3 failed');
}
};
  function main() {
  test_rec_insertion_sort();
  $numbers = [5, 3, 4, 1, 2];
  $numbers = rec_insertion_sort($numbers, count($numbers));
  echo rtrim(_str($numbers)), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
