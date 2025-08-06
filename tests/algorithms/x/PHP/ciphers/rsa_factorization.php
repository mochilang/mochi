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
  function gcd($a, $b) {
  $x = $a;
  $y = $b;
  while ($y != 0) {
  $t = $x % $y;
  $x = $y;
  $y = $t;
};
  if ($x < 0) {
  return -$x;
}
  return $x;
};
  function pow_mod($base, $exp, $mod) {
  $result = 1;
  $b = $base % $mod;
  $e = $exp;
  while ($e > 0) {
  if ($e % 2 == 1) {
  $result = ($result * $b) % $mod;
}
  $e = _intdiv($e, 2);
  $b = ($b * $b) % $mod;
};
  return $result;
};
  function rsa_factor($d, $e, $n) {
  $k = $d * $e - 1;
  $p = 0;
  $q = 0;
  $g = 2;
  while ($p == 0 && $g < $n) {
  $t = $k;
  while ($t % 2 == 0) {
  $t = _intdiv($t, 2);
  $x = pow_mod($g, $t, $n);
  $y = gcd($x - 1, $n);
  if ($x > 1 && $y > 1) {
  $p = $y;
  $q = _intdiv($n, $y);
  break;
}
};
  $g = $g + 1;
};
  if ($p > $q) {
  return [$q, $p];
}
  return [$p, $q];
};
  echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(rsa_factor(3, 16971, 25777), 1344))))))), PHP_EOL;
  echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(rsa_factor(7331, 11, 27233), 1344))))))), PHP_EOL;
  echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(rsa_factor(4021, 13, 17711), 1344))))))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
