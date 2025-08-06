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
function sqrtApprox($x) {
  global $PI, $GAUSSIAN_KERNEL, $SOBEL_GX, $SOBEL_GY, $image, $edges;
  $guess = $x / 2.0;
  $i = 0;
  while ($i < 20) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
}
function atanApprox($x) {
  global $PI, $GAUSSIAN_KERNEL, $SOBEL_GX, $SOBEL_GY, $image, $edges;
  if ($x > 1.0) {
  return $PI / 2.0 - $x / ($x * $x + 0.28);
}
  if ($x < (-1.0)) {
  return -$PI / 2.0 - $x / ($x * $x + 0.28);
}
  return $x / (1.0 + 0.28 * $x * $x);
}
function atan2Approx($y, $x) {
  global $PI, $GAUSSIAN_KERNEL, $SOBEL_GX, $SOBEL_GY, $image, $edges;
  if ($x > 0.0) {
  $r = atanApprox($y / $x);
  return $r;
}
  if ($x < 0.0) {
  if ($y >= 0.0) {
  return atanApprox($y / $x) + $PI;
};
  return atanApprox($y / $x) - $PI;
}
  if ($y > 0.0) {
  return $PI / 2.0;
}
  if ($y < 0.0) {
  return -$PI / 2.0;
}
  return 0.0;
}
function deg($rad) {
  global $PI, $GAUSSIAN_KERNEL, $SOBEL_GX, $SOBEL_GY, $image, $edges;
  return $rad * 180.0 / $PI;
}
$GAUSSIAN_KERNEL = [[0.0625, 0.125, 0.0625], [0.125, 0.25, 0.125], [0.0625, 0.125, 0.0625]];
$SOBEL_GX = [[-1.0, 0.0, 1.0], [-2.0, 0.0, 2.0], [-1.0, 0.0, 1.0]];
$SOBEL_GY = [[1.0, 2.0, 1.0], [0.0, 0.0, 0.0], [-1.0, -2.0, -1.0]];
function zero_matrix($h, $w) {
  global $PI, $GAUSSIAN_KERNEL, $SOBEL_GX, $SOBEL_GY, $image, $edges;
  $out = [];
  $i = 0;
  while ($i < $h) {
  $row = [];
  $j = 0;
  while ($j < $w) {
  $row = _append($row, 0.0);
  $j = $j + 1;
};
  $out = _append($out, $row);
  $i = $i + 1;
};
  return $out;
}
function convolve($img, $kernel) {
  global $PI, $GAUSSIAN_KERNEL, $SOBEL_GX, $SOBEL_GY, $image, $edges;
  $h = count($img);
  $w = count($img[0]);
  $k = count($kernel);
  $pad = _intdiv($k, 2);
  $out = zero_matrix($h, $w);
  $y = $pad;
  while ($y < $h - $pad) {
  $x = $pad;
  while ($x < $w - $pad) {
  $sum = 0.0;
  $ky = 0;
  while ($ky < $k) {
  $kx = 0;
  while ($kx < $k) {
  $pixel = $img[$y - $pad + $ky][$x - $pad + $kx];
  $weight = $kernel[$ky][$kx];
  $sum = $sum + $pixel * $weight;
  $kx = $kx + 1;
};
  $ky = $ky + 1;
};
  $out[$y][$x] = $sum;
  $x = $x + 1;
};
  $y = $y + 1;
};
  return $out;
}
function gaussian_blur($img) {
  global $PI, $GAUSSIAN_KERNEL, $SOBEL_GX, $SOBEL_GY, $image, $edges;
  return convolve($img, $GAUSSIAN_KERNEL);
}
function sobel_filter($img) {
  global $PI, $GAUSSIAN_KERNEL, $SOBEL_GX, $SOBEL_GY, $image, $edges;
  $gx = convolve($img, $SOBEL_GX);
  $gy = convolve($img, $SOBEL_GY);
  $h = count($img);
  $w = count($img[0]);
  $grad = zero_matrix($h, $w);
  $dir = zero_matrix($h, $w);
  $i = 0;
  while ($i < $h) {
  $j = 0;
  while ($j < $w) {
  $gxx = $gx[$i][$j];
  $gyy = $gy[$i][$j];
  $grad[$i][$j] = sqrtApprox($gxx * $gxx + $gyy * $gyy);
  $dir[$i][$j] = deg(atan2Approx($gyy, $gxx)) + 180.0;
  $j = $j + 1;
};
  $i = $i + 1;
};
  return ['grad' => $grad, 'dir' => $dir];
}
function suppress_non_maximum($h, $w, $direction, $grad) {
  global $PI, $GAUSSIAN_KERNEL, $SOBEL_GX, $SOBEL_GY, $image, $edges;
  $dest = zero_matrix($h, $w);
  $r = 1;
  while ($r < $h - 1) {
  $c = 1;
  while ($c < $w - 1) {
  $angle = $direction[$r][$c];
  $q = 0.0;
  $p = 0.0;
  if (($angle >= 0.0 && $angle < 22.5) || ($angle >= 157.5 && $angle <= 180.0) || ($angle >= 337.5)) {
  $q = $grad[$r][$c + 1];
  $p = $grad[$r][$c - 1];
} else {
  if (($angle >= 22.5 && $angle < 67.5) || ($angle >= 202.5 && $angle < 247.5)) {
  $q = $grad[$r + 1][$c - 1];
  $p = $grad[$r - 1][$c + 1];
} else {
  if (($angle >= 67.5 && $angle < 112.5) || ($angle >= 247.5 && $angle < 292.5)) {
  $q = $grad[$r + 1][$c];
  $p = $grad[$r - 1][$c];
} else {
  $q = $grad[$r - 1][$c - 1];
  $p = $grad[$r + 1][$c + 1];
};
};
}
  if ($grad[$r][$c] >= $q && $grad[$r][$c] >= $p) {
  $dest[$r][$c] = $grad[$r][$c];
}
  $c = $c + 1;
};
  $r = $r + 1;
};
  return $dest;
}
function double_threshold($h, $w, &$img, $low, $high, $weak, $strong) {
  global $PI, $GAUSSIAN_KERNEL, $SOBEL_GX, $SOBEL_GY, $image, $edges;
  $r = 0;
  while ($r < $h) {
  $c = 0;
  while ($c < $w) {
  $v = $img[$r][$c];
  if ($v >= $high) {
  $img[$r][$c] = $strong;
} else {
  if ($v < $low) {
  $img[$r][$c] = 0.0;
} else {
  $img[$r][$c] = $weak;
};
}
  $c = $c + 1;
};
  $r = $r + 1;
};
}
function track_edge($h, $w, &$img, $weak, $strong) {
  global $PI, $GAUSSIAN_KERNEL, $SOBEL_GX, $SOBEL_GY, $image, $edges;
  $r = 1;
  while ($r < $h - 1) {
  $c = 1;
  while ($c < $w - 1) {
  if ($img[$r][$c] == $weak) {
  if ($img[$r + 1][$c] == $strong || $img[$r - 1][$c] == $strong || $img[$r][$c + 1] == $strong || $img[$r][$c - 1] == $strong || $img[$r - 1][$c - 1] == $strong || $img[$r - 1][$c + 1] == $strong || $img[$r + 1][$c - 1] == $strong || $img[$r + 1][$c + 1] == $strong) {
  $img[$r][$c] = $strong;
} else {
  $img[$r][$c] = 0.0;
};
}
  $c = $c + 1;
};
  $r = $r + 1;
};
}
function canny($image, $low, $high, $weak, $strong) {
  global $PI, $GAUSSIAN_KERNEL, $SOBEL_GX, $SOBEL_GY, $edges;
  $blurred = gaussian_blur($image);
  $sob = sobel_filter($blurred);
  $grad = $sob['grad'];
  $direction = $sob['dir'];
  $h = count($image);
  $w = count($image[0]);
  $suppressed = suppress_non_maximum($h, $w, $direction, $grad);
  double_threshold($h, $w, $suppressed, $low, $high, $weak, $strong);
  track_edge($h, $w, $suppressed, $weak, $strong);
  return $suppressed;
}
function print_image($img) {
  global $PI, $GAUSSIAN_KERNEL, $SOBEL_GX, $SOBEL_GY, $image, $edges;
  $r = 0;
  while ($r < count($img)) {
  $c = 0;
  $line = '';
  while ($c < count($img[$r])) {
  $line = $line . _str(intval($img[$r][$c])) . ' ';
  $c = $c + 1;
};
  echo rtrim($line), PHP_EOL;
  $r = $r + 1;
};
}
$image = [[0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 255.0, 255.0, 255.0, 0.0], [0.0, 255.0, 255.0, 255.0, 0.0], [0.0, 255.0, 255.0, 255.0, 0.0], [0.0, 0.0, 0.0, 0.0, 0.0]];
$edges = canny($image, 20.0, 40.0, 128.0, 255.0);
print_image($edges);
