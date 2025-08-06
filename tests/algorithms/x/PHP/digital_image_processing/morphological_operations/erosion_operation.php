<?php
ini_set('memory_limit', '-1');
function _str($x) {
    if (is_array($x)) {
        $isList = array_keys($x) === range(0, count($x) - 1);
        if ($isList) {
            $parts = [];
            foreach ($x as $v) { $parts[] = _str($v); }
            return '[' . implode(' ', $parts) . ']';
        }
        $parts = [];
        foreach ($x as $k => $v) { $parts[] = _str($k) . ':' . _str($v); }
        return 'map[' . implode(' ', $parts) . ']';
    }
    if (is_bool($x)) return $x ? 'true' : 'false';
    if ($x === null) return 'null';
    return strval($x);
}
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
function rgb_to_gray($rgb) {
  global $rgb_img, $gray_img, $img1, $kernel1, $img2, $kernel2;
  $gray = [];
  $i = 0;
  while ($i < count($rgb)) {
  $row = [];
  $j = 0;
  while ($j < count($rgb[$i])) {
  $r = floatval($rgb[$i][$j][0]);
  $g = floatval($rgb[$i][$j][1]);
  $b = floatval($rgb[$i][$j][2]);
  $value = 0.2989 * $r + 0.587 * $g + 0.114 * $b;
  $row = _append($row, $value);
  $j = $j + 1;
};
  $gray = _append($gray, $row);
  $i = $i + 1;
};
  return $gray;
}
function gray_to_binary($gray) {
  global $rgb_img, $gray_img, $img1, $kernel1, $img2, $kernel2;
  $binary = [];
  $i = 0;
  while ($i < count($gray)) {
  $row = [];
  $j = 0;
  while ($j < count($gray[$i])) {
  $row = _append($row, $gray[$i][$j] > 127.0 && $gray[$i][$j] <= 255.0);
  $j = $j + 1;
};
  $binary = _append($binary, $row);
  $i = $i + 1;
};
  return $binary;
}
function erosion($image, $kernel) {
  global $rgb_img, $gray_img, $img1, $kernel1, $img2, $kernel2;
  $h = count($image);
  $w = count($image[0]);
  $k_h = count($kernel);
  $k_w = count($kernel[0]);
  $pad_y = _intdiv($k_h, 2);
  $pad_x = _intdiv($k_w, 2);
  $padded = [];
  $y = 0;
  while ($y < $h + 2 * $pad_y) {
  $row = [];
  $x = 0;
  while ($x < $w + 2 * $pad_x) {
  $row = _append($row, false);
  $x = $x + 1;
};
  $padded = _append($padded, $row);
  $y = $y + 1;
};
  $y = 0;
  while ($y < $h) {
  $x = 0;
  while ($x < $w) {
  $padded[$pad_y + $y][$pad_x + $x] = $image[$y][$x];
  $x = $x + 1;
};
  $y = $y + 1;
};
  $output = [];
  $y = 0;
  while ($y < $h) {
  $row_out = [];
  $x = 0;
  while ($x < $w) {
  $sum = 0;
  $ky = 0;
  while ($ky < $k_h) {
  $kx = 0;
  while ($kx < $k_w) {
  if ($kernel[$ky][$kx] == 1 && $padded[$y + $ky][$x + $kx]) {
  $sum = $sum + 1;
}
  $kx = $kx + 1;
};
  $ky = $ky + 1;
};
  $row_out = _append($row_out, $sum == 5);
  $x = $x + 1;
};
  $output = _append($output, $row_out);
  $y = $y + 1;
};
  return $output;
}
$rgb_img = [[[127, 255, 0]]];
echo rtrim(_str(rgb_to_gray($rgb_img))), PHP_EOL;
$gray_img = [[127.0, 255.0, 0.0]];
echo rtrim(_str(gray_to_binary($gray_img))), PHP_EOL;
$img1 = [[true, true, false]];
$kernel1 = [[0, 1, 0]];
echo rtrim(_str(erosion($img1, $kernel1))), PHP_EOL;
$img2 = [[true, false, false]];
$kernel2 = [[1, 1, 0]];
echo rtrim(_str(erosion($img2, $kernel2))), PHP_EOL;
