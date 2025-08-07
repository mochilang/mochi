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
  function binary_exponentiation($a, $n, $mod) {
  global $p, $left, $right_fast, $right_naive;
  if ($n == 0) {
  return 1;
}
  if ($n % 2 == 1) {
  return fmod((binary_exponentiation($a, $n - 1, $mod) * $a), $mod);
}
  $b = binary_exponentiation($a, _intdiv($n, 2), $mod);
  return ($b * $b) % $mod;
};
  function naive_exponent_mod($a, $n, $mod) {
  global $p, $b, $left, $right_fast, $right_naive;
  $result = 1;
  $i = 0;
  while ($i < $n) {
  $result = ($result * $a) % $mod;
  $i = $i + 1;
};
  return $result;
};
  function print_bool($b) {
  global $p, $a, $left, $right_fast, $right_naive;
  if ($b) {
  echo rtrim((true ? 'true' : 'false')), PHP_EOL;
} else {
  echo rtrim((false ? 'true' : 'false')), PHP_EOL;
}
};
  $p = 701;
  $a = 1000000000;
  $b = 10;
  $left = (_intdiv($a, $b)) % $p;
  $right_fast = fmod(($a * binary_exponentiation($b, $p - 2, $p)), $p);
  print_bool($left == $right_fast);
  $right_naive = fmod(($a * naive_exponent_mod($b, $p - 2, $p)), $p);
  print_bool($left == $right_naive);
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
