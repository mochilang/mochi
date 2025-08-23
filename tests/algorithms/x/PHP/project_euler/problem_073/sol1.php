<?php
error_reporting(E_ALL & ~E_DEPRECATED);
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
function _intdiv($a, $b) {
    if ($b === 0 || $b === '0') {
        throw new DivisionByZeroError();
    }
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv(intval($a), intval($b));
}
$__start_mem = memory_get_usage();
$__start = _now();
  function gcd($a, $b) {
  $x = $a;
  $y = $b;
  while ($y != 0) {
  $temp = $x % $y;
  $x = $y;
  $y = $temp;
};
  return $x;
};
  function solution($max_d) {
  $fractions_number = 0;
  $d = 0;
  while ($d <= $max_d) {
  $n = _intdiv($d, 3) + 1;
  $half = _intdiv(($d + 1), 2);
  while ($n < $half) {
  if (gcd($n, $d) == 1) {
  $fractions_number = $fractions_number + 1;
}
  $n = $n + 1;
};
  $d = $d + 1;
};
  return $fractions_number;
};
  function main() {
  echo rtrim(json_encode(solution(12000), 1344)), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
