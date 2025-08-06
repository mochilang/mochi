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
$PI = 3.141592653589793;
function absf($x) {
  global $PI;
  if ($x < 0.0) {
  return -$x;
}
  return $x;
}
function sqrtApprox($x) {
  global $PI;
  if ($x <= 0.0) {
  return 0.0;
}
  $guess = $x / 2.0;
  $i = 0;
  while ($i < 20) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
}
function atanApprox($x) {
  global $PI;
  if ($x > 1.0) {
  return $PI / 2.0 - $x / ($x * $x + 0.28);
}
  if ($x < (-1.0)) {
  return -$PI / 2.0 - $x / ($x * $x + 0.28);
}
  return $x / (1.0 + 0.28 * $x * $x);
}
function atan2Approx($y, $x) {
  global $PI;
  if ($x == 0.0) {
  if ($y > 0.0) {
  return $PI / 2.0;
};
  if ($y < 0.0) {
  return -$PI / 2.0;
};
  return 0.0;
}
  $a = atanApprox($y / $x);
  if ($x > 0.0) {
  return $a;
}
  if ($y >= 0.0) {
  return $a + $PI;
}
  return $a - $PI;
}
function zeros($h, $w) {
  global $PI;
  $m = [];
  $y = 0;
  while ($y < $h) {
  $row = [];
  $x = 0;
  while ($x < $w) {
  $row = _append($row, 0.0);
  $x = $x + 1;
};
  $m = _append($m, $row);
  $y = $y + 1;
};
  return $m;
}
function pad_edge($img, $pad) {
  global $PI;
  $h = count($img);
  $w = count($img[0]);
  $out = zeros($h + $pad * 2, $w + $pad * 2);
  $y = 0;
  while ($y < $h + $pad * 2) {
  $x = 0;
  while ($x < $w + $pad * 2) {
  $sy = $y - $pad;
  if ($sy < 0) {
  $sy = 0;
}
  if ($sy >= $h) {
  $sy = $h - 1;
}
  $sx = $x - $pad;
  if ($sx < 0) {
  $sx = 0;
}
  if ($sx >= $w) {
  $sx = $w - 1;
}
  $out[$y][$x] = $img[$sy][$sx];
  $x = $x + 1;
};
  $y = $y + 1;
};
  return $out;
}
function img_convolve($img, $kernel) {
  global $PI;
  $h = count($img);
  $w = count($img[0]);
  $k = count($kernel);
  $pad = _intdiv($k, 2);
  $padded = pad_edge($img, $pad);
  $out = zeros($h, $w);
  $y = 0;
  while ($y < $h) {
  $x = 0;
  while ($x < $w) {
  $sum = 0.0;
  $i = 0;
  while ($i < $k) {
  $j = 0;
  while ($j < $k) {
  $sum = $sum + $padded[$y + $i][$x + $j] * (floatval($kernel[$i][$j]));
  $j = $j + 1;
};
  $i = $i + 1;
};
  $out[$y][$x] = $sum;
  $x = $x + 1;
};
  $y = $y + 1;
};
  return $out;
}
function abs_matrix($mat) {
  global $PI;
  $h = count($mat);
  $w = count($mat[0]);
  $out = zeros($h, $w);
  $y = 0;
  while ($y < $h) {
  $x = 0;
  while ($x < $w) {
  $v = $mat[$y][$x];
  if ($v < 0.0) {
  $out[$y][$x] = -$v;
} else {
  $out[$y][$x] = $v;
}
  $x = $x + 1;
};
  $y = $y + 1;
};
  return $out;
}
function max_matrix($mat) {
  global $PI;
  $max_val = $mat[0][0];
  $y = 0;
  while ($y < count($mat)) {
  $x = 0;
  while ($x < count($mat[0])) {
  if ($mat[$y][$x] > $max_val) {
  $max_val = $mat[$y][$x];
}
  $x = $x + 1;
};
  $y = $y + 1;
};
  return $max_val;
}
function scale_matrix($mat, $factor) {
  global $PI;
  $h = count($mat);
  $w = count($mat[0]);
  $out = zeros($h, $w);
  $y = 0;
  while ($y < $h) {
  $x = 0;
  while ($x < $w) {
  $out[$y][$x] = $mat[$y][$x] * $factor;
  $x = $x + 1;
};
  $y = $y + 1;
};
  return $out;
}
function sobel_filter($image) {
  global $PI;
  $h = count($image);
  $w = count($image[0]);
  $img = [];
  $y0 = 0;
  while ($y0 < $h) {
  $row = [];
  $x0 = 0;
  while ($x0 < $w) {
  $row = _append($row, floatval($image[$y0][$x0]));
  $x0 = $x0 + 1;
};
  $img = _append($img, $row);
  $y0 = $y0 + 1;
};
  $kernel_x = [[-1, 0, 1], [-2, 0, 2], [-1, 0, 1]];
  $kernel_y = [[1, 2, 1], [0, 0, 0], [-1, -2, -1]];
  $dst_x = abs_matrix(img_convolve($img, $kernel_x));
  $dst_y = abs_matrix(img_convolve($img, $kernel_y));
  $max_x = max_matrix($dst_x);
  $max_y = max_matrix($dst_y);
  $dst_x = scale_matrix($dst_x, 255.0 / $max_x);
  $dst_y = scale_matrix($dst_y, 255.0 / $max_y);
  $mag = zeros($h, $w);
  $theta = zeros($h, $w);
  $y = 0;
  while ($y < $h) {
  $x = 0;
  while ($x < $w) {
  $gx = $dst_x[$y][$x];
  $gy = $dst_y[$y][$x];
  $mag[$y][$x] = sqrtApprox($gx * $gx + $gy * $gy);
  $theta[$y][$x] = atan2Approx($gy, $gx);
  $x = $x + 1;
};
  $y = $y + 1;
};
  $max_m = max_matrix($mag);
  $mag = scale_matrix($mag, 255.0 / $max_m);
  return [$mag, $theta];
}
function print_matrix_int($mat) {
  global $PI;
  $y = 0;
  while ($y < count($mat)) {
  $line = '';
  $x = 0;
  while ($x < count($mat[$y])) {
  $line = $line . _str(intval($mat[$y][$x]));
  if ($x < count($mat[$y]) - 1) {
  $line = $line . ' ';
}
  $x = $x + 1;
};
  echo rtrim($line), PHP_EOL;
  $y = $y + 1;
};
}
function print_matrix_float($mat) {
  global $PI;
  $y = 0;
  while ($y < count($mat)) {
  $line = '';
  $x = 0;
  while ($x < count($mat[$y])) {
  $line = $line . _str($mat[$y][$x]);
  if ($x < count($mat[$y]) - 1) {
  $line = $line . ' ';
}
  $x = $x + 1;
};
  echo rtrim($line), PHP_EOL;
  $y = $y + 1;
};
}
function main() {
  global $PI;
  $img = [[10, 10, 10, 10, 10], [10, 50, 50, 50, 10], [10, 50, 80, 50, 10], [10, 50, 50, 50, 10], [10, 10, 10, 10, 10]];
  $res = sobel_filter($img);
  $mag = $res[0];
  $theta = $res[1];
  print_matrix_int($mag);
  print_matrix_float($theta);
}
main();
