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
function make_list($n, $value) {
  global $img, $result;
  $res = [];
  $i = 0;
  while ($i < $n) {
  $res = _append($res, $value);
  $i = $i + 1;
};
  return $res;
}
function histogram_stretch(&$image) {
  global $img, $result;
  $height = count($image);
  $width = count($image[0]);
  $hist = make_list(256, 0);
  $i = 0;
  while ($i < $height) {
  $j = 0;
  while ($j < $width) {
  $val = $image[$i][$j];
  $hist[$val] = $hist[$val] + 1;
  $j = $j + 1;
};
  $i = $i + 1;
};
  $mapping = make_list(256, 0);
  $cumulative = 0;
  $total = $height * $width;
  $h = 0;
  while ($h < 256) {
  $cumulative = $cumulative + $hist[$h];
  $mapping[$h] = _intdiv((255 * $cumulative), $total);
  $h = $h + 1;
};
  $i = 0;
  while ($i < $height) {
  $j = 0;
  while ($j < $width) {
  $val = $image[$i][$j];
  $image[$i][$j] = $mapping[$val];
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $image;
}
function print_image($image) {
  global $img, $result;
  $i = 0;
  while ($i < count($image)) {
  echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($image[$i], 1344))))))), PHP_EOL;
  $i = $i + 1;
};
}
$img = [[52, 55, 61], [59, 79, 61], [85, 76, 62]];
$result = histogram_stretch($img);
print_image($result);
