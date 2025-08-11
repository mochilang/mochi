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
  function odd_even_sort($xs) {
  $arr = [];
  $i = 0;
  while ($i < count($xs)) {
  $arr = _append($arr, $xs[$i]);
  $i = _iadd($i, 1);
};
  $n = count($arr);
  $sorted = false;
  while ($sorted == false) {
  $sorted = true;
  $j = 0;
  while ($j < _isub($n, 1)) {
  if ($arr[$j] > $arr[_iadd($j, 1)]) {
  $tmp = $arr[$j];
  $arr[$j] = $arr[_iadd($j, 1)];
  $arr[_iadd($j, 1)] = $tmp;
  $sorted = false;
}
  $j = _iadd($j, 2);
};
  $j = 1;
  while ($j < _isub($n, 1)) {
  if ($arr[$j] > $arr[_iadd($j, 1)]) {
  $tmp = $arr[$j];
  $arr[$j] = $arr[_iadd($j, 1)];
  $arr[_iadd($j, 1)] = $tmp;
  $sorted = false;
}
  $j = _iadd($j, 2);
};
};
  return $arr;
};
  function print_list($xs) {
  $i = 0;
  $out = '';
  while ($i < count($xs)) {
  if ($i > 0) {
  $out = $out . ' ';
}
  $out = $out . _str($xs[$i]);
  $i = _iadd($i, 1);
};
  echo rtrim($out), PHP_EOL;
};
  function test_odd_even_sort() {
  $a = [5, 4, 3, 2, 1];
  $r1 = odd_even_sort($a);
  if ($r1[0] != 1 || $r1[1] != 2 || $r1[2] != 3 || $r1[3] != 4 || $r1[4] != 5) {
  _panic('case1 failed');
}
  $b = [];
  $r2 = odd_even_sort($b);
  if (count($r2) != 0) {
  _panic('case2 failed');
}
  $c = [-10, -1, 10, 2];
  $r3 = odd_even_sort($c);
  if ($r3[0] != (-10) || $r3[1] != (-1) || $r3[2] != 2 || $r3[3] != 10) {
  _panic('case3 failed');
}
  $d = [1, 2, 3, 4];
  $r4 = odd_even_sort($d);
  if ($r4[0] != 1 || $r4[1] != 2 || $r4[2] != 3 || $r4[3] != 4) {
  _panic('case4 failed');
}
};
  function main() {
  test_odd_even_sort();
  $sample = [5, 4, 3, 2, 1];
  $sorted = odd_even_sort($sample);
  print_list($sorted);
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
