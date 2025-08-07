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
  function is_prime($n) {
  if ($n <= 1) {
  return false;
}
  if ($n <= 3) {
  return true;
}
  if ($n % 2 == 0) {
  return false;
}
  $i = 3;
  while ($i * $i <= $n) {
  if ($n % $i == 0) {
  return false;
}
  $i = $i + 2;
};
  return true;
};
  function is_germain_prime($number) {
  if ($number < 1) {
  $panic('Input value must be a positive integer');
}
  return is_prime($number) && is_prime(2 * $number + 1);
};
  function is_safe_prime($number) {
  if ($number < 1) {
  $panic('Input value must be a positive integer');
}
  if (($number - 1) % 2 != 0) {
  return false;
}
  return is_prime($number) && is_prime(_intdiv(($number - 1), 2));
};
  function test_is_germain_prime() {
  if (!is_germain_prime(3)) {
  $panic('is_germain_prime(3) failed');
}
  if (!is_germain_prime(11)) {
  $panic('is_germain_prime(11) failed');
}
  if (is_germain_prime(4)) {
  $panic('is_germain_prime(4) failed');
}
  if (!is_germain_prime(23)) {
  $panic('is_germain_prime(23) failed');
}
  if (is_germain_prime(13)) {
  $panic('is_germain_prime(13) failed');
}
  if (is_germain_prime(20)) {
  $panic('is_germain_prime(20) failed');
}
};
  function test_is_safe_prime() {
  if (!is_safe_prime(5)) {
  $panic('is_safe_prime(5) failed');
}
  if (!is_safe_prime(11)) {
  $panic('is_safe_prime(11) failed');
}
  if (is_safe_prime(1)) {
  $panic('is_safe_prime(1) failed');
}
  if (is_safe_prime(2)) {
  $panic('is_safe_prime(2) failed');
}
  if (is_safe_prime(3)) {
  $panic('is_safe_prime(3) failed');
}
  if (!is_safe_prime(47)) {
  $panic('is_safe_prime(47) failed');
}
};
  function main() {
  test_is_germain_prime();
  test_is_safe_prime();
  echo rtrim(json_encode(is_germain_prime(23), 1344)), PHP_EOL;
  echo rtrim(json_encode(is_safe_prime(47), 1344)), PHP_EOL;
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
