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
  function binary_exp_recursive($base, $exponent) {
  if ($exponent < 0) {
  $panic('exponent must be non-negative');
}
  if ($exponent == 0) {
  return 1.0;
}
  if ($exponent % 2 == 1) {
  return binary_exp_recursive($base, $exponent - 1) * $base;
}
  $half = binary_exp_recursive($base, _intdiv($exponent, 2));
  return $half * $half;
};
  function binary_exp_iterative($base, $exponent) {
  if ($exponent < 0) {
  $panic('exponent must be non-negative');
}
  $result = 1.0;
  $b = $base;
  $e = $exponent;
  while ($e > 0) {
  if ($e % 2 == 1) {
  $result = $result * $b;
}
  $b = $b * $b;
  $e = _intdiv($e, 2);
};
  return $result;
};
  function binary_exp_mod_recursive($base, $exponent, $modulus) {
  if ($exponent < 0) {
  $panic('exponent must be non-negative');
}
  if ($modulus <= 0) {
  $panic('modulus must be positive');
}
  if ($exponent == 0) {
  return 1 % $modulus;
}
  if ($exponent % 2 == 1) {
  return fmod((binary_exp_mod_recursive($base, $exponent - 1, $modulus) * ($base % $modulus)), $modulus);
}
  $r = binary_exp_mod_recursive($base, _intdiv($exponent, 2), $modulus);
  return ($r * $r) % $modulus;
};
  function binary_exp_mod_iterative($base, $exponent, $modulus) {
  if ($exponent < 0) {
  $panic('exponent must be non-negative');
}
  if ($modulus <= 0) {
  $panic('modulus must be positive');
}
  $result = 1 % $modulus;
  $b = $base % $modulus;
  $e = $exponent;
  while ($e > 0) {
  if ($e % 2 == 1) {
  $result = ($result * $b) % $modulus;
}
  $b = ($b * $b) % $modulus;
  $e = _intdiv($e, 2);
};
  return $result;
};
  echo rtrim(json_encode(binary_exp_recursive(3.0, 5), 1344)), PHP_EOL;
  echo rtrim(json_encode(binary_exp_iterative(1.5, 4), 1344)), PHP_EOL;
  echo rtrim(json_encode(binary_exp_mod_recursive(3, 4, 5), 1344)), PHP_EOL;
  echo rtrim(json_encode(binary_exp_mod_iterative(11, 13, 7), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
