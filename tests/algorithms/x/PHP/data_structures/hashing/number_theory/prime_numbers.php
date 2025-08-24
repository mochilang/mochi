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
$__start_mem = memory_get_usage();
$__start = _now();
  function isPrime($number) {
  if ($number < 2) {
  return false;
}
  if ($number < 4) {
  return true;
}
  if ($number % 2 == 0) {
  return false;
}
  $i = 3;
  while ($i * $i <= $number) {
  if ($number % $i == 0) {
  return false;
}
  $i = $i + 2;
};
  return true;
};
  function nextPrime($value, $factor, $desc) {
  $v = $value * $factor;
  $firstValue = $v;
  while (!isPrime($v)) {
  if ($desc) {
  $v = $v - 1;
} else {
  $v = $v + 1;
}
};
  if ($v == $firstValue) {
  if ($desc) {
  return nextPrime($v - 1, 1, $desc);
} else {
  return nextPrime($v + 1, 1, $desc);
};
}
  return $v;
};
  echo rtrim(json_encode(isPrime(0), 1344)), PHP_EOL;
  echo rtrim(json_encode(isPrime(1), 1344)), PHP_EOL;
  echo rtrim(json_encode(isPrime(2), 1344)), PHP_EOL;
  echo rtrim(json_encode(isPrime(3), 1344)), PHP_EOL;
  echo rtrim(json_encode(isPrime(27), 1344)), PHP_EOL;
  echo rtrim(json_encode(isPrime(87), 1344)), PHP_EOL;
  echo rtrim(json_encode(isPrime(563), 1344)), PHP_EOL;
  echo rtrim(json_encode(isPrime(2999), 1344)), PHP_EOL;
  echo rtrim(json_encode(isPrime(67483), 1344)), PHP_EOL;
  echo rtrim(json_encode(nextPrime(14, 1, false), 1344)), PHP_EOL;
  echo rtrim(json_encode(nextPrime(14, 1, true), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
