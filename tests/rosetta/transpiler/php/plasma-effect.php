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
  function floorf($x) {
  global $PI, $nframes, $w, $h, $total, $f, $y, $fx, $fy, $value, $rem, $ci;
  $i = intval($x);
  if ((floatval($i)) > $x) {
  $i = $i - 1;
}
  return floatval($i);
};
  function frac($x) {
  global $PI, $nframes, $w, $h, $total, $f, $y, $fx, $fy, $value, $rem, $ci;
  return $x - floorf($x);
};
  function sinApprox($x) {
  global $PI, $nframes, $w, $h, $total, $f, $y, $fx, $fy, $value, $rem, $ci;
  $term = $x;
  $sum = $x;
  $n = 1;
  while ($n <= 10) {
  $denom = floatval(((2 * $n) * (2 * $n + 1)));
  $term = -$term * $x * $x / $denom;
  $sum = $sum + $term;
  $n = $n + 1;
};
  return $sum;
};
  function sqrtApprox($x) {
  global $PI, $nframes, $w, $h, $total, $f, $y, $fx, $fy, $value, $rem, $ci;
  if ($x <= 0) {
  return 0.0;
}
  $guess = $x;
  $i = 0;
  while ($i < 10) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
};
  $nframes = 10;
  $w = 32;
  $h = 32;
  $total = 0;
  $f = 1;
  while ($f <= $nframes) {
  $y = 0;
  while ($y < $h) {
  $x = 0;
  while ($x < $w) {
  $fx = floatval($x);
  $fy = floatval($y);
  $value = sinApprox($fx / 16.0);
  $value = $value + sinApprox($fy / 8.0);
  $value = $value + sinApprox(($fx + $fy) / 16.0);
  $value = $value + sinApprox(sqrtApprox($fx * $fx + $fy * $fy) / 8.0);
  $value = $value + 4.0;
  $value = $value / 8.0;
  $rem = frac($value + (floatval($f)) / (floatval($nframes)));
  $ci = intval((floatval($nframes) * $rem)) + 1;
  $total = $total + $ci;
  $x = $x + 1;
};
  $y = $y + 1;
};
  $f = $f + 1;
}
  echo rtrim(json_encode($total, 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
