<?php
ini_set('memory_limit', '-1');
$PI = 3.141592653589793;
function mochi_abs($x) {
  global $PI;
  if ($x < 0.0) {
  return -$x;
}
  return $x;
}
function to_radians($deg) {
  global $PI;
  return $deg * $PI / 180.0;
}
function sin_taylor($x) {
  global $PI;
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
  global $PI;
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
function rect($mag, $angle) {
  global $PI;
  $c = cos_taylor($angle);
  $s = sin_taylor($angle);
  return [$mag * $c, $mag * $s];
}
function multiply($a, $b) {
  global $PI;
  return [$a[0] * $b[0] - $a[1] * $b[1], $a[0] * $b[1] + $a[1] * $b[0]];
}
function apparent_power($voltage, $current, $voltage_angle, $current_angle) {
  global $PI;
  $vrad = to_radians($voltage_angle);
  $irad = to_radians($current_angle);
  $vrect = rect($voltage, $vrad);
  $irect = rect($current, $irad);
  $result = multiply($vrect, $irect);
  return $result;
}
function approx_equal($a, $b, $eps) {
  global $PI;
  return mochi_abs($a[0] - $b[0]) < $eps && mochi_abs($a[1] - $b[1]) < $eps;
}
