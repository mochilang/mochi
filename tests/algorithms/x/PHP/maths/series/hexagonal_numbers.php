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
$__start_mem = memory_get_usage();
$__start = _now();
  function hexagonal_numbers($length) {
  if ($length <= 0) {
  $panic('Length must be a positive integer.');
}
  $res = [];
  $n = 0;
  while ($n < $length) {
  $res = _append($res, $n * (2 * $n - 1));
  $n = $n + 1;
};
  return $res;
};
  function test_hexagonal_numbers() {
  $expected5 = [0, 1, 6, 15, 28];
  $result5 = hexagonal_numbers(5);
  if ($result5 != $expected5) {
  $panic('hexagonal_numbers(5) failed');
}
  $expected10 = [0, 1, 6, 15, 28, 45, 66, 91, 120, 153];
  $result10 = hexagonal_numbers(10);
  if ($result10 != $expected10) {
  $panic('hexagonal_numbers(10) failed');
}
};
  test_hexagonal_numbers();
  echo rtrim(_str(hexagonal_numbers(5))), PHP_EOL;
  echo rtrim(_str(hexagonal_numbers(10))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
