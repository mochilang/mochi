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
  function sqrtApprox($x) {
  global $GAUSSIAN_KERNEL, $PI, $SOBEL_GX, $SOBEL_GY, $edges, $image;
  $guess = $x / 2.0;
  $i = 0;
  while ($i < 20) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
};
  function atanApprox($x) {
  global $GAUSSIAN_KERNEL, $PI, $SOBEL_GX, $SOBEL_GY, $edges, $image;
  if ($x > 1.0) {
  return $PI / 2.0 - $x / ($x * $x + 0.28);
}
  if ($x < (-1.0)) {
  return -$PI / 2.0 - $x / ($x * $x + 0.28);
}
  return $x / (1.0 + 0.28 * $x * $x);
};
  function atan2Approx($y, $x) {
  global $GAUSSIAN_KERNEL, $PI, $SOBEL_GX, $SOBEL_GY, $edges, $image;
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
};
  function deg($rad) {
  global $GAUSSIAN_KERNEL, $PI, $SOBEL_GX, $SOBEL_GY, $edges, $image;
  return $rad * 180.0 / $PI;
};
  $GAUSSIAN_KERNEL = [[0.0625, 0.125, 0.0625], [0.125, 0.25, 0.125], [0.0625, 0.125, 0.0625]];
  $SOBEL_GX = [[-1.0, 0.0, 1.0], [-2.0, 0.0, 2.0], [-1.0, 0.0, 1.0]];
  $SOBEL_GY = [[1.0, 2.0, 1.0], [0.0, 0.0, 0.0], [-1.0, -2.0, -1.0]];
  function zero_matrix($h, $w) {
  global $GAUSSIAN_KERNEL, $PI, $SOBEL_GX, $SOBEL_GY, $edges, $image;
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
};
  function convolve($img, $kernel) {
  global $GAUSSIAN_KERNEL, $PI, $SOBEL_GX, $SOBEL_GY, $edges, $image;
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
};
  function gaussian_blur($img) {
  global $GAUSSIAN_KERNEL, $PI, $SOBEL_GX, $SOBEL_GY, $edges, $image;
  return convolve($img, $GAUSSIAN_KERNEL);
};
  function sobel_filter($img) {
  global $GAUSSIAN_KERNEL, $PI, $SOBEL_GX, $SOBEL_GY, $edges, $image;
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
  return ['dir' => $dir, 'grad' => $grad];
};
  function suppress_non_maximum($h, $w, $direction, $grad) {
  global $GAUSSIAN_KERNEL, $PI, $SOBEL_GX, $SOBEL_GY, $edges, $image;
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
};
  function double_threshold($h, $w, &$img, $low, $high, $weak, $strong) {
  global $GAUSSIAN_KERNEL, $PI, $SOBEL_GX, $SOBEL_GY, $edges, $image;
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
};
  function track_edge($h, $w, &$img, $weak, $strong) {
  global $GAUSSIAN_KERNEL, $PI, $SOBEL_GX, $SOBEL_GY, $edges, $image;
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
};
  function canny($image, $low, $high, $weak, $strong) {
  global $GAUSSIAN_KERNEL, $PI, $SOBEL_GX, $SOBEL_GY, $edges;
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
};
  function print_image($img) {
  global $GAUSSIAN_KERNEL, $PI, $SOBEL_GX, $SOBEL_GY, $edges, $image;
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
};
  $image = [[0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 255.0, 255.0, 255.0, 0.0], [0.0, 255.0, 255.0, 255.0, 0.0], [0.0, 255.0, 255.0, 255.0, 0.0], [0.0, 0.0, 0.0, 0.0, 0.0]];
  $edges = canny($image, 20.0, 40.0, 128.0, 255.0);
  print_image($edges);
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
