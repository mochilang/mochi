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
  function pad_edge($image, $pad_size) {
  global $laplace_kernel, $result;
  $height = count($image);
  $width = count($image[0]);
  $new_height = $height + $pad_size * 2;
  $new_width = $width + $pad_size * 2;
  $padded = [];
  $i = 0;
  while ($i < $new_height) {
  $row = [];
  $src_i = $i;
  if ($src_i < $pad_size) {
  $src_i = 0;
}
  if ($src_i >= $height + $pad_size) {
  $src_i = $height - 1;
} else {
  $src_i = $src_i - $pad_size;
}
  $j = 0;
  while ($j < $new_width) {
  $src_j = $j;
  if ($src_j < $pad_size) {
  $src_j = 0;
}
  if ($src_j >= $width + $pad_size) {
  $src_j = $width - 1;
} else {
  $src_j = $src_j - $pad_size;
}
  $row = _append($row, $image[$src_i][$src_j]);
  $j = $j + 1;
};
  $padded = _append($padded, $row);
  $i = $i + 1;
};
  return $padded;
};
  function im2col($image, $block_h, $block_w) {
  global $laplace_kernel, $result;
  $rows = count($image);
  $cols = count($image[0]);
  $dst_height = $rows - $block_h + 1;
  $dst_width = $cols - $block_w + 1;
  $image_array = [];
  $i = 0;
  while ($i < $dst_height) {
  $j = 0;
  while ($j < $dst_width) {
  $window = [];
  $bi = 0;
  while ($bi < $block_h) {
  $bj = 0;
  while ($bj < $block_w) {
  $window = _append($window, $image[$i + $bi][$j + $bj]);
  $bj = $bj + 1;
};
  $bi = $bi + 1;
};
  $image_array = _append($image_array, $window);
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $image_array;
};
  function flatten($matrix) {
  global $image, $laplace_kernel, $result;
  $out = [];
  $i = 0;
  while ($i < count($matrix)) {
  $j = 0;
  while ($j < count($matrix[$i])) {
  $out = _append($out, $matrix[$i][$j]);
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $out;
};
  function dot($a, $b) {
  global $image, $laplace_kernel, $result;
  $sum = 0;
  $i = 0;
  while ($i < count($a)) {
  $sum = $sum + $a[$i] * $b[$i];
  $i = $i + 1;
};
  return $sum;
};
  function img_convolve($image, $kernel) {
  global $laplace_kernel, $result;
  $height = count($image);
  $width = count($image[0]);
  $k_size = count($kernel);
  $pad_size = _intdiv($k_size, 2);
  $padded = pad_edge($image, $pad_size);
  $image_array = im2col($padded, $k_size, $k_size);
  $kernel_flat = flatten($kernel);
  $dst = [];
  $idx = 0;
  $i = 0;
  while ($i < $height) {
  $row = [];
  $j = 0;
  while ($j < $width) {
  $val = dot($image_array[$idx], $kernel_flat);
  $row = _append($row, $val);
  $idx = $idx + 1;
  $j = $j + 1;
};
  $dst = _append($dst, $row);
  $i = $i + 1;
};
  return $dst;
};
  function print_matrix($m) {
  global $image, $laplace_kernel, $result;
  $i = 0;
  while ($i < count($m)) {
  $line = '';
  $j = 0;
  while ($j < count($m[$i])) {
  if ($j > 0) {
  $line = $line . ' ';
}
  $line = $line . _str($m[$i][$j]);
  $j = $j + 1;
};
  echo rtrim($line), PHP_EOL;
  $i = $i + 1;
};
};
  $image = [[1, 2, 3, 0, 0], [4, 5, 6, 0, 0], [7, 8, 9, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]];
  $laplace_kernel = [[0, 1, 0], [1, -4, 1], [0, 1, 0]];
  $result = img_convolve($image, $laplace_kernel);
  print_matrix($result);
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
