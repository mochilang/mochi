<?php
ini_set('memory_limit', '-1');
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
$PI = 3.141592653589793;
function to_radians($deg) {
  global $PI, $kernel;
  return $deg * $PI / 180.0;
}
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
}
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
}
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
}
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
}
$kernel = gabor_filter_kernel(3, 8.0, 0.0, 10.0, 0.0, 0.0);
echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($kernel, 1344))))))), PHP_EOL;
