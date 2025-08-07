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
  function sock_merchant($colors) {
  $arr = [];
  $i = 0;
  while ($i < count($colors)) {
  $arr = _append($arr, $colors[$i]);
  $i = $i + 1;
};
  $n = count($arr);
  $a = 0;
  while ($a < $n) {
  $min_idx = $a;
  $b = $a + 1;
  while ($b < $n) {
  if ($arr[$b] < $arr[$min_idx]) {
  $min_idx = $b;
}
  $b = $b + 1;
};
  $temp = $arr[$a];
  $arr[$a] = $arr[$min_idx];
  $arr[$min_idx] = $temp;
  $a = $a + 1;
};
  $pairs = 0;
  $i = 0;
  while ($i < $n) {
  $count = 1;
  while ($i + 1 < $n && $arr[$i] == $arr[$i + 1]) {
  $count = $count + 1;
  $i = $i + 1;
};
  $pairs = $pairs + _intdiv($count, 2);
  $i = $i + 1;
};
  return $pairs;
};
  function test_sock_merchant() {
  $example1 = [10, 20, 20, 10, 10, 30, 50, 10, 20];
  if (sock_merchant($example1) != 3) {
  $panic('example1 failed');
}
  $example2 = [1, 1, 3, 3];
  if (sock_merchant($example2) != 2) {
  $panic('example2 failed');
}
};
  function main() {
  test_sock_merchant();
  $example1 = [10, 20, 20, 10, 10, 30, 50, 10, 20];
  echo rtrim(_str(sock_merchant($example1))), PHP_EOL;
  $example2 = [1, 1, 3, 3];
  echo rtrim(_str(sock_merchant($example2))), PHP_EOL;
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
