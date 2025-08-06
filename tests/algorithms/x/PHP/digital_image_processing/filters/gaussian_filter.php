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
function expApprox($x) {
  global $PI, $img, $gaussian3, $gaussian5;
  $sum = 1.0;
  $term = 1.0;
  $n = 1;
  while ($n < 10) {
  $term = $term * $x / (floatval($n));
  $sum = $sum + $term;
  $n = $n + 1;
};
  return $sum;
}
function gen_gaussian_kernel($k_size, $sigma) {
  global $PI, $img, $gaussian3, $gaussian5;
  $center = _intdiv($k_size, 2);
  $kernel = [];
  $i = 0;
  while ($i < $k_size) {
  $row = [];
  $j = 0;
  while ($j < $k_size) {
  $x = floatval(($i - $center));
  $y = floatval(($j - $center));
  $exponent = -(($x * $x + $y * $y) / (2.0 * $sigma * $sigma));
  $value = (1.0 / (2.0 * $PI * $sigma)) * expApprox($exponent);
  $row = _append($row, $value);
  $j = $j + 1;
};
  $kernel = _append($kernel, $row);
  $i = $i + 1;
};
  return $kernel;
}
function gaussian_filter($image, $k_size, $sigma) {
  global $PI, $img, $gaussian3, $gaussian5;
  $height = count($image);
  $width = count($image[0]);
  $dst_height = $height - $k_size + 1;
  $dst_width = $width - $k_size + 1;
  $kernel = gen_gaussian_kernel($k_size, $sigma);
  $dst = [];
  $i = 0;
  while ($i < $dst_height) {
  $row = [];
  $j = 0;
  while ($j < $dst_width) {
  $sum = 0.0;
  $ki = 0;
  while ($ki < $k_size) {
  $kj = 0;
  while ($kj < $k_size) {
  $sum = $sum + (floatval($image[$i + $ki][$j + $kj])) * $kernel[$ki][$kj];
  $kj = $kj + 1;
};
  $ki = $ki + 1;
};
  $row = _append($row, intval($sum));
  $j = $j + 1;
};
  $dst = _append($dst, $row);
  $i = $i + 1;
};
  return $dst;
}
function print_image($image) {
  global $PI, $img, $gaussian3, $gaussian5;
  $i = 0;
  while ($i < count($image)) {
  echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($image[$i], 1344))))))), PHP_EOL;
  $i = $i + 1;
};
}
$img = [[52, 55, 61, 59, 79], [62, 59, 55, 104, 94], [63, 65, 66, 113, 144], [68, 70, 70, 126, 154], [70, 72, 69, 128, 155]];
$gaussian3 = gaussian_filter($img, 3, 1.0);
$gaussian5 = gaussian_filter($img, 5, 0.8);
print_image($gaussian3);
print_image($gaussian5);
