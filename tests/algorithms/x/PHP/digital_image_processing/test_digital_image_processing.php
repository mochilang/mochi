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
function clamp_byte($x) {
  global $img, $negative, $contrast, $kernel, $laplace, $convolved, $medianed, $sobel, $lbp_img;
  if ($x < 0) {
  return 0;
}
  if ($x > 255) {
  return 255;
}
  return $x;
}
function convert_to_negative($img) {
  global $negative, $contrast, $kernel, $laplace, $convolved, $medianed, $sobel, $lbp_img;
  $h = count($img);
  $w = count($img[0]);
  $out = [];
  $y = 0;
  while ($y < $h) {
  $row = [];
  $x = 0;
  while ($x < $w) {
  $row = _append($row, 255 - $img[$y][$x]);
  $x = $x + 1;
};
  $out = _append($out, $row);
  $y = $y + 1;
};
  return $out;
}
function change_contrast($img, $factor) {
  global $negative, $contrast, $kernel, $laplace, $convolved, $medianed, $sobel, $lbp_img;
  $h = count($img);
  $w = count($img[0]);
  $out = [];
  $y = 0;
  while ($y < $h) {
  $row = [];
  $x = 0;
  while ($x < $w) {
  $p = $img[$y][$x];
  $v = _intdiv((($p - 128) * $factor), 100) + 128;
  $v = clamp_byte($v);
  $row = _append($row, $v);
  $x = $x + 1;
};
  $out = _append($out, $row);
  $y = $y + 1;
};
  return $out;
}
function gen_gaussian_kernel($n, $sigma) {
  global $img, $negative, $contrast, $kernel, $laplace, $convolved, $medianed, $sobel, $lbp_img;
  if ($n == 3) {
  return [[1.0 / 16.0, 2.0 / 16.0, 1.0 / 16.0], [2.0 / 16.0, 4.0 / 16.0, 2.0 / 16.0], [1.0 / 16.0, 2.0 / 16.0, 1.0 / 16.0]];
}
  $k = [];
  $i = 0;
  while ($i < $n) {
  $row = [];
  $j = 0;
  while ($j < $n) {
  $row = _append($row, 0.0);
  $j = $j + 1;
};
  $k = _append($k, $row);
  $i = $i + 1;
};
  return $k;
}
function img_convolve($img, $kernel) {
  global $negative, $contrast, $laplace, $convolved, $medianed, $sobel, $lbp_img;
  $h = count($img);
  $w = count($img[0]);
  $out = [];
  $y = 0;
  while ($y < $h) {
  $row = [];
  $x = 0;
  while ($x < $w) {
  $acc = 0.0;
  $ky = 0;
  while ($ky < count($kernel)) {
  $kx = 0;
  while ($kx < count($kernel[0])) {
  $iy = $y + $ky - 1;
  $ix = $x + $kx - 1;
  $pixel = 0;
  if ($iy >= 0 && $iy < $h && $ix >= 0 && $ix < $w) {
  $pixel = $img[$iy][$ix];
}
  $acc = $acc + $kernel[$ky][$kx] * (1.0 * $pixel);
  $kx = $kx + 1;
};
  $ky = $ky + 1;
};
  $row = _append($row, intval($acc));
  $x = $x + 1;
};
  $out = _append($out, $row);
  $y = $y + 1;
};
  return $out;
}
function sort_ints($xs) {
  global $img, $negative, $contrast, $kernel, $laplace, $convolved, $medianed, $sobel, $lbp_img;
  $arr = $xs;
  $i = 0;
  while ($i < count($arr)) {
  $j = 0;
  while ($j < count($arr) - 1 - $i) {
  if ($arr[$j] > $arr[$j + 1]) {
  $tmp = $arr[$j];
  $arr[$j] = $arr[$j + 1];
  $arr[$j + 1] = $tmp;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $arr;
}
function median_filter($img, $k) {
  global $negative, $contrast, $kernel, $laplace, $convolved, $medianed, $sobel, $lbp_img;
  $h = count($img);
  $w = count($img[0]);
  $offset = _intdiv($k, 2);
  $out = [];
  $y = 0;
  while ($y < $h) {
  $row = [];
  $x = 0;
  while ($x < $w) {
  $vals = [];
  $ky = 0;
  while ($ky < $k) {
  $kx = 0;
  while ($kx < $k) {
  $iy = $y + $ky - $offset;
  $ix = $x + $kx - $offset;
  $pixel = 0;
  if ($iy >= 0 && $iy < $h && $ix >= 0 && $ix < $w) {
  $pixel = $img[$iy][$ix];
}
  $vals = _append($vals, $pixel);
  $kx = $kx + 1;
};
  $ky = $ky + 1;
};
  $sorted = sort_ints($vals);
  $row = _append($row, $sorted[count($sorted) / 2]);
  $x = $x + 1;
};
  $out = _append($out, $row);
  $y = $y + 1;
};
  return $out;
}
function iabs($x) {
  global $img, $negative, $contrast, $kernel, $laplace, $convolved, $medianed, $sobel, $lbp_img;
  if ($x < 0) {
  return -$x;
}
  return $x;
}
function sobel_filter($img) {
  global $negative, $contrast, $kernel, $laplace, $convolved, $medianed, $sobel, $lbp_img;
  $gx = [[1, 0, -1], [2, 0, -2], [1, 0, -1]];
  $gy = [[1, 2, 1], [0, 0, 0], [-1, -2, -1]];
  $h = count($img);
  $w = count($img[0]);
  $out = [];
  $y = 0;
  while ($y < $h) {
  $row = [];
  $x = 0;
  while ($x < $w) {
  $sx = 0;
  $sy = 0;
  $ky = 0;
  while ($ky < 3) {
  $kx = 0;
  while ($kx < 3) {
  $iy = $y + $ky - 1;
  $ix = $x + $kx - 1;
  $pixel = 0;
  if ($iy >= 0 && $iy < $h && $ix >= 0 && $ix < $w) {
  $pixel = $img[$iy][$ix];
}
  $sx = $sx + $gx[$ky][$kx] * $pixel;
  $sy = $sy + $gy[$ky][$kx] * $pixel;
  $kx = $kx + 1;
};
  $ky = $ky + 1;
};
  $row = _append($row, iabs($sx) + iabs($sy));
  $x = $x + 1;
};
  $out = _append($out, $row);
  $y = $y + 1;
};
  return $out;
}
function get_neighbors_pixel($img, $x, $y) {
  global $negative, $contrast, $kernel, $laplace, $convolved, $medianed, $sobel, $lbp_img;
  $h = count($img);
  $w = count($img[0]);
  $neighbors = [];
  $dy = -1;
  while ($dy <= 1) {
  $dx = -1;
  while ($dx <= 1) {
  if (!($dx == 0 && $dy == 0)) {
  $ny = $y + $dy;
  $nx = $x + $dx;
  $val = 0;
  if ($ny >= 0 && $ny < $h && $nx >= 0 && $nx < $w) {
  $val = $img[$ny][$nx];
};
  $neighbors = _append($neighbors, $val);
}
  $dx = $dx + 1;
};
  $dy = $dy + 1;
};
  return $neighbors;
}
function pow2($e) {
  global $img, $negative, $contrast, $kernel, $laplace, $convolved, $medianed, $sobel, $lbp_img;
  $r = 1;
  $i = 0;
  while ($i < $e) {
  $r = $r * 2;
  $i = $i + 1;
};
  return $r;
}
function local_binary_value($img, $x, $y) {
  global $negative, $contrast, $kernel, $laplace, $convolved, $medianed, $sobel, $lbp_img;
  $center = $img[$y][$x];
  $neighbors = get_neighbors_pixel($img, $x, $y);
  $v = 0;
  $i = 0;
  while ($i < count($neighbors)) {
  if ($neighbors[$i] >= $center) {
  $v = $v + pow2($i);
}
  $i = $i + 1;
};
  return $v;
}
function local_binary_pattern($img) {
  global $negative, $contrast, $kernel, $laplace, $convolved, $medianed, $sobel, $lbp_img;
  $h = count($img);
  $w = count($img[0]);
  $out = [];
  $y = 0;
  while ($y < $h) {
  $row = [];
  $x = 0;
  while ($x < $w) {
  $row = _append($row, local_binary_value($img, $x, $y));
  $x = $x + 1;
};
  $out = _append($out, $row);
  $y = $y + 1;
};
  return $out;
}
$img = [[52, 55, 61], [62, 59, 55], [63, 65, 66]];
$negative = convert_to_negative($img);
$contrast = change_contrast($img, 110);
$kernel = gen_gaussian_kernel(3, 1.0);
$laplace = [[0.25, 0.5, 0.25], [0.5, -3.0, 0.5], [0.25, 0.5, 0.25]];
$convolved = img_convolve($img, $laplace);
$medianed = median_filter($img, 3);
$sobel = sobel_filter($img);
$lbp_img = local_binary_pattern($img);
echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($negative, 1344))))))), PHP_EOL;
echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($contrast, 1344))))))), PHP_EOL;
echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($kernel, 1344))))))), PHP_EOL;
echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($convolved, 1344))))))), PHP_EOL;
echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($medianed, 1344))))))), PHP_EOL;
echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($sobel, 1344))))))), PHP_EOL;
echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($lbp_img, 1344))))))), PHP_EOL;
