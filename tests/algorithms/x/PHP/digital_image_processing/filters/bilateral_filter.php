<?php
ini_set('memory_limit', '-1');
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$PI = 3.141592653589793;
function mochi_abs($x) {
  global $PI, $img, $result;
  if ($x < 0.0) {
  return -$x;
}
  return $x;
}
function sqrtApprox($x) {
  global $PI, $img, $result;
  if ($x <= 0.0) {
  return 0.0;
}
  $guess = $x;
  $i = 0;
  while ($i < 10) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
}
function expApprox($x) {
  global $PI, $img, $result;
  $term = 1.0;
  $sum = 1.0;
  $n = 1;
  while ($n < 10) {
  $term = $term * $x / (floatval($n));
  $sum = $sum + $term;
  $n = $n + 1;
};
  return $sum;
}
function vec_gaussian($mat, $variance) {
  global $PI, $img, $result;
  $i = 0;
  $out = [];
  while ($i < count($mat)) {
  $row = [];
  $j = 0;
  while ($j < count($mat[$i])) {
  $v = $mat[$i][$j];
  $e = -($v * $v) / (2.0 * $variance);
  $row = array_merge($row, [expApprox($e)]);
  $j = $j + 1;
};
  $out = array_merge($out, [$row]);
  $i = $i + 1;
};
  return $out;
}
function get_slice($img, $x, $y, $kernel_size) {
  global $PI, $result;
  $half = _intdiv($kernel_size, 2);
  $i = $x - $half;
  $slice = [];
  while ($i <= $x + $half) {
  $row = [];
  $j = $y - $half;
  while ($j <= $y + $half) {
  $row = array_merge($row, [$img[$i][$j]]);
  $j = $j + 1;
};
  $slice = array_merge($slice, [$row]);
  $i = $i + 1;
};
  return $slice;
}
function get_gauss_kernel($kernel_size, $spatial_variance) {
  global $PI, $img, $result;
  $arr = [];
  $i = 0;
  while ($i < $kernel_size) {
  $row = [];
  $j = 0;
  while ($j < $kernel_size) {
  $di = floatval(($i - _intdiv($kernel_size, 2)));
  $dj = floatval(($j - _intdiv($kernel_size, 2)));
  $dist = sqrtApprox($di * $di + $dj * $dj);
  $row = array_merge($row, [$dist]);
  $j = $j + 1;
};
  $arr = array_merge($arr, [$row]);
  $i = $i + 1;
};
  return vec_gaussian($arr, $spatial_variance);
}
function elementwise_sub($mat, $value) {
  global $PI, $img, $result;
  $res = [];
  $i = 0;
  while ($i < count($mat)) {
  $row = [];
  $j = 0;
  while ($j < count($mat[$i])) {
  $row = array_merge($row, [$mat[$i][$j] - $value]);
  $j = $j + 1;
};
  $res = array_merge($res, [$row]);
  $i = $i + 1;
};
  return $res;
}
function elementwise_mul($a, $b) {
  global $PI, $img, $result;
  $res = [];
  $i = 0;
  while ($i < count($a)) {
  $row = [];
  $j = 0;
  while ($j < count($a[$i])) {
  $row = array_merge($row, [$a[$i][$j] * $b[$i][$j]]);
  $j = $j + 1;
};
  $res = array_merge($res, [$row]);
  $i = $i + 1;
};
  return $res;
}
function matrix_sum($mat) {
  global $PI, $img, $result;
  $total = 0.0;
  $i = 0;
  while ($i < count($mat)) {
  $j = 0;
  while ($j < count($mat[$i])) {
  $total = $total + $mat[$i][$j];
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $total;
}
function bilateral_filter($img, $spatial_variance, $intensity_variance, $kernel_size) {
  global $PI, $result;
  $gauss_ker = get_gauss_kernel($kernel_size, $spatial_variance);
  $img_s = $img;
  $center = $img_s[_intdiv($kernel_size, 2)][_intdiv($kernel_size, 2)];
  $img_i = elementwise_sub($img_s, $center);
  $img_ig = vec_gaussian($img_i, $intensity_variance);
  $weights = elementwise_mul($gauss_ker, $img_ig);
  $vals = elementwise_mul($img_s, $weights);
  $sum_weights = matrix_sum($weights);
  $val = 0.0;
  if ($sum_weights != 0.0) {
  $val = matrix_sum($vals) / $sum_weights;
}
  return $val;
}
$img = [[0.2, 0.3, 0.4], [0.3, 0.4, 0.5], [0.4, 0.5, 0.6]];
$result = bilateral_filter($img, 1.0, 1.0, 3);
echo rtrim(json_encode($result, 1344)), PHP_EOL;
