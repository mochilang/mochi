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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
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
  $PI = 3.141592653589793;
  function to_radians($deg) {
  global $PI, $kernel;
  return $deg * $PI / 180.0;
};
  function sin_taylor($x) {
  global $PI, $kernel;
  $term = $x;
  $sum = $x;
  $i = 1;
  while ($i < 10) {
  $k1 = 2.0 * (floatval($i));
  $k2 = $k1 + 1.0;
  $term = -$term * $x * $x / ($k1 * $k2);
  $sum = $sum + $term;
  $i = $i + 1;
};
  return $sum;
};
  function cos_taylor($x) {
  global $PI, $kernel;
  $term = 1.0;
  $sum = 1.0;
  $i = 1;
  while ($i < 10) {
  $k1 = 2.0 * (floatval($i)) - 1.0;
  $k2 = 2.0 * (floatval($i));
  $term = -$term * $x * $x / ($k1 * $k2);
  $sum = $sum + $term;
  $i = $i + 1;
};
  return $sum;
};
  function exp_taylor($x) {
  global $PI, $kernel;
  $term = 1.0;
  $sum = 1.0;
  $i = 1.0;
  while ($i < 20.0) {
  $term = $term * $x / $i;
  $sum = $sum + $term;
  $i = $i + 1.0;
};
  return $sum;
};
  function gabor_filter_kernel($ksize, $sigma, $theta, $lambd, $gamma, $psi) {
  global $PI, $kernel;
  $size = $ksize;
  if ($size % 2 == 0) {
  $size = $size + 1;
}
  $gabor = [];
  $y = 0;
  while ($y < $size) {
  $row = [];
  $x = 0;
  while ($x < $size) {
  $px = floatval(($x - _intdiv($size, 2)));
  $py = floatval(($y - _intdiv($size, 2)));
  $rad = to_radians($theta);
  $cos_theta = cos_taylor($rad);
  $sin_theta = sin_taylor($rad);
  $x_rot = $cos_theta * $px + $sin_theta * $py;
  $y_rot = -$sin_theta * $px + $cos_theta * $py;
  $exponent = -($x_rot * $x_rot + $gamma * $gamma * $y_rot * $y_rot) / (2.0 * $sigma * $sigma);
  $value = exp_taylor($exponent) * cos_taylor(2.0 * $PI * $x_rot / $lambd + $psi);
  $row = _append($row, $value);
  $x = $x + 1;
};
  $gabor = _append($gabor, $row);
  $y = $y + 1;
};
  return $gabor;
};
  $kernel = gabor_filter_kernel(3, 8.0, 0.0, 10.0, 0.0, 0.0);
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($kernel, 1344)))))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
