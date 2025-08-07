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
  function is_happy_number($num) {
  if ($num <= 0) {
  $panic('num must be a positive integer');
}
  $seen = [];
  $n = $num;
  while ($n != 1) {
  $i = 0;
  while ($i < count($seen)) {
  if ($seen[$i] == $n) {
  return false;
}
  $i = $i + 1;
};
  $seen = _append($seen, $n);
  $total = 0;
  $temp = $n;
  while ($temp > 0) {
  $digit = $temp % 10;
  $total = $total + $digit * $digit;
  $temp = _intdiv($temp, 10);
};
  $n = $total;
};
  return true;
};
  function test_is_happy_number() {
  if (!is_happy_number(19)) {
  $panic('19 should be happy');
}
  if (is_happy_number(2)) {
  $panic('2 should be unhappy');
}
  if (!is_happy_number(23)) {
  $panic('23 should be happy');
}
  if (!is_happy_number(1)) {
  $panic('1 should be happy');
}
};
  function main() {
  test_is_happy_number();
  echo rtrim(json_encode(is_happy_number(19), 1344)), PHP_EOL;
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
