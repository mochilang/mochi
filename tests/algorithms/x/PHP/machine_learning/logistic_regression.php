<?php
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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function _iadd($a, $b) {
    if (function_exists('bcadd')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcadd($sa, $sb, 0);
    }
    return $a + $b;
}
function _isub($a, $b) {
    if (function_exists('bcsub')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcsub($sa, $sb, 0);
    }
    return $a - $b;
}
function _imul($a, $b) {
    if (function_exists('bcmul')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcmul($sa, $sb, 0);
    }
    return $a * $b;
}
function _idiv($a, $b) {
    return _intdiv($a, $b);
}
function _imod($a, $b) {
    if (function_exists('bcmod')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcmod($sa, $sb));
    }
    return $a % $b;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function expApprox($x) {
  global $alpha, $iterations, $theta;
  $y = $x;
  $is_neg = false;
  if ($x < 0.0) {
  $is_neg = true;
  $y = -$x;
}
  $term = 1.0;
  $sum = 1.0;
  $n = 1;
  while ($n < 30) {
  $term = $term * $y / (floatval($n));
  $sum = $sum + $term;
  $n = _iadd($n, 1);
};
  if ($is_neg) {
  return 1.0 / $sum;
}
  return $sum;
};
  function sigmoid($z) {
  global $x, $y, $alpha, $iterations, $theta;
  return 1.0 / (1.0 + expApprox(-$z));
};
  function dot($a, $b) {
  global $x, $y, $alpha, $iterations, $theta;
  $s = 0.0;
  $i = 0;
  while ($i < count($a)) {
  $s = $s + $a[$i] * $b[$i];
  $i = _iadd($i, 1);
};
  return $s;
};
  function zeros($n) {
  global $x, $y, $alpha, $iterations, $theta;
  $res = [];
  $i = 0;
  while ($i < $n) {
  $res = _append($res, 0.0);
  $i = _iadd($i, 1);
};
  return $res;
};
  function logistic_reg($alpha, $x, $y, $iterations) {
  $m = count($x);
  $n = count($x[0]);
  $theta = zeros($n);
  $iter = 0;
  while ($iter < $iterations) {
  $grad = zeros($n);
  $i = 0;
  while ($i < $m) {
  $z = dot($x[$i], $theta);
  $h = sigmoid($z);
  $k = 0;
  while ($k < $n) {
  $grad[$k] = $grad[$k] + ($h - $y[$i]) * $x[$i][$k];
  $k = _iadd($k, 1);
};
  $i = _iadd($i, 1);
};
  $k2 = 0;
  while ($k2 < $n) {
  $theta[$k2] = $theta[$k2] - $alpha * $grad[$k2] / (floatval($m));
  $k2 = _iadd($k2, 1);
};
  $iter = _iadd($iter, 1);
};
  return $theta;
};
  $x = [[0.5, 1.5], [1.0, 1.0], [1.5, 0.5], [3.0, 3.5], [3.5, 3.0], [4.0, 4.0]];
  $y = [0.0, 0.0, 0.0, 1.0, 1.0, 1.0];
  $alpha = 0.1;
  $iterations = 1000;
  $theta = logistic_reg($alpha, $x, $y, $iterations);
  for ($i = 0; $i < count($theta); $i++) {
  echo rtrim(json_encode($theta[$i], 1344)), PHP_EOL;
}
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
