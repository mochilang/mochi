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
$__start_mem = memory_get_usage();
$__start = _now();
  $PI = 3.141592653589793;
  function absf($x) {
  global $PI;
  if ($x < 0.0) {
  return -$x;
}
  return $x;
};
  function sqrt($x) {
  global $PI;
  if ($x < 0.0) {
  $panic('sqrt domain error');
}
  $guess = $x / 2.0;
  $i = 0;
  while ($i < 20) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
};
  function ln($x) {
  global $PI;
  if ($x <= 0.0) {
  $panic('ln domain error');
}
  $y = ($x - 1.0) / ($x + 1.0);
  $y2 = $y * $y;
  $term = $y;
  $sum = 0.0;
  $k = 0;
  while ($k < 10) {
  $denom = floatval((2 * $k + 1));
  $sum = $sum + $term / $denom;
  $term = $term * $y2;
  $k = $k + 1;
};
  return 2.0 * $sum;
};
  function exp_series($x) {
  global $PI;
  $term = 1.0;
  $sum = 1.0;
  $n = 1;
  while ($n < 20) {
  $term = $term * $x / (floatval($n));
  $sum = $sum + $term;
  $n = $n + 1;
};
  return $sum;
};
  function powf($base, $exponent) {
  global $PI;
  if ($base <= 0.0) {
  return 0.0;
}
  return exp_series($exponent * ln($base));
};
  function integrand($x, $z) {
  global $PI;
  return powf($x, $z - 1.0) * exp_series(-$x);
};
  function gamma_iterative($num) {
  global $PI;
  if ($num <= 0.0) {
  $panic('math domain error');
}
  $step = 0.001;
  $limit = 100.0;
  $x = $step;
  $total = 0.0;
  while ($x < $limit) {
  $total = $total + integrand($x, $num) * $step;
  $x = $x + $step;
};
  return $total;
};
  function gamma_recursive($num) {
  global $PI;
  if ($num <= 0.0) {
  $panic('math domain error');
}
  if ($num > 171.5) {
  $panic('math range error');
}
  $int_part = intval($num);
  $frac = $num - (floatval($int_part));
  if (!(absf($frac) < 0.000001 || absf($frac - 0.5) < 0.000001)) {
  $panic('num must be an integer or a half-integer');
}
  if (absf($num - 0.5) < 0.000001) {
  return sqrt($PI);
}
  if (absf($num - 1.0) < 0.000001) {
  return 1.0;
}
  return ($num - 1.0) * gamma_recursive($num - 1.0);
};
  function main() {
  global $PI;
  echo rtrim(json_encode(gamma_iterative(5.0), 1344)), PHP_EOL;
  echo rtrim(json_encode(gamma_recursive(5.0), 1344)), PHP_EOL;
  echo rtrim(json_encode(gamma_recursive(0.5), 1344)), PHP_EOL;
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
