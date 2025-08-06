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
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function maxpooling($arr, $size, $stride) {
  $n = count($arr);
  if ($n == 0 || count($arr[0]) != $n) {
  $panic('The input array is not a square matrix');
}
  $result = [];
  $i = 0;
  while ($i + $size <= $n) {
  $row = [];
  $j = 0;
  while ($j + $size <= $n) {
  $max_val = $arr[$i][$j];
  $r = $i;
  while ($r < $i + $size) {
  $c = $j;
  while ($c < $j + $size) {
  $val = $arr[$r][$c];
  if ($val > $max_val) {
  $max_val = $val;
}
  $c = $c + 1;
};
  $r = $r + 1;
};
  $row = _append($row, $max_val);
  $j = $j + $stride;
};
  $result = _append($result, $row);
  $i = $i + $stride;
};
  return $result;
};
  function avgpooling($arr, $size, $stride) {
  $n = count($arr);
  if ($n == 0 || count($arr[0]) != $n) {
  $panic('The input array is not a square matrix');
}
  $result = [];
  $i = 0;
  while ($i + $size <= $n) {
  $row = [];
  $j = 0;
  while ($j + $size <= $n) {
  $sum = 0;
  $r = $i;
  while ($r < $i + $size) {
  $c = $j;
  while ($c < $j + $size) {
  $sum = $sum + $arr[$r][$c];
  $c = $c + 1;
};
  $r = $r + 1;
};
  $row = _append($row, _intdiv($sum, ($size * $size)));
  $j = $j + $stride;
};
  $result = _append($result, $row);
  $i = $i + $stride;
};
  return $result;
};
  function print_matrix($mat) {
  $i = 0;
  while ($i < count($mat)) {
  $line = '';
  $j = 0;
  while ($j < count($mat[$i])) {
  $line = $line . _str($mat[$i][$j]);
  if ($j < count($mat[$i]) - 1) {
  $line = $line . ' ';
}
  $j = $j + 1;
};
  echo rtrim($line), PHP_EOL;
  $i = $i + 1;
};
};
  function main() {
  $arr1 = [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12], [13, 14, 15, 16]];
  $arr2 = [[147, 180, 122], [241, 76, 32], [126, 13, 157]];
  print_matrix(maxpooling($arr1, 2, 2));
  print_matrix(maxpooling($arr2, 2, 1));
  print_matrix(avgpooling($arr1, 2, 2));
  print_matrix(avgpooling($arr2, 2, 1));
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
