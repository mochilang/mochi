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
  function odd_sieve($num) {
  if ($num <= 2) {
  return [];
}
  if ($num == 3) {
  return [2];
}
  $size = _intdiv($num, 2) - 1;
  $sieve = [];
  $idx = 0;
  while ($idx < $size) {
  $sieve = _append($sieve, true);
  $idx = $idx + 1;
};
  $i = 3;
  while ($i * $i <= $num) {
  $s_idx = _intdiv($i, 2) - 1;
  if ($sieve[$s_idx]) {
  $j = $i * $i;
  while ($j < $num) {
  $j_idx = _intdiv($j, 2) - 1;
  $sieve[$j_idx] = false;
  $j = $j + 2 * $i;
};
}
  $i = $i + 2;
};
  $primes = [2];
  $n = 3;
  $k = 0;
  while ($n < $num) {
  if ($sieve[$k]) {
  $primes = _append($primes, $n);
}
  $n = $n + 2;
  $k = $k + 1;
};
  return $primes;
};
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(odd_sieve(2), 1344)))))), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(odd_sieve(3), 1344)))))), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(odd_sieve(10), 1344)))))), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(odd_sieve(20), 1344)))))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
