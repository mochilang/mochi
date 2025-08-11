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
function _intdiv($a, $b) {
    if ($b === 0 || $b === '0') {
        throw new DivisionByZeroError();
    }
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
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
  $PI = 3.141592653589793;
  function sinApprox($x) {
  global $PI, $sample_rate, $size, $audio, $t, $coeffs;
  $term = $x;
  $sum = $x;
  $n = 1;
  while ($n <= 10) {
  $denom = floatval((_imul((_imul(2, $n)), (_iadd(_imul(2, $n), 1)))));
  $term = -$term * $x * $x / $denom;
  $sum = $sum + $term;
  $n = _iadd($n, 1);
};
  return $sum;
};
  function cosApprox($x) {
  global $PI, $sample_rate, $size, $audio, $t, $coeffs;
  $term = 1.0;
  $sum = 1.0;
  $n = 1;
  while ($n <= 10) {
  $denom = floatval((_imul((_isub(_imul(2, $n), 1)), (_imul(2, $n)))));
  $term = -$term * $x * $x / $denom;
  $sum = $sum + $term;
  $n = _iadd($n, 1);
};
  return $sum;
};
  function expApprox($x) {
  global $PI, $sample_rate, $size, $audio, $t, $coeffs;
  $sum = 1.0;
  $term = 1.0;
  $n = 1;
  while ($n < 10) {
  $term = $term * $x / (floatval($n));
  $sum = $sum + $term;
  $n = _iadd($n, 1);
};
  return $sum;
};
  function ln($x) {
  global $PI, $sample_rate, $size, $audio, $coeffs;
  $t = ($x - 1.0) / ($x + 1.0);
  $term = $t;
  $sum = 0.0;
  $n = 1;
  while ($n <= 19) {
  $sum = $sum + $term / (floatval($n));
  $term = $term * $t * $t;
  $n = _iadd($n, 2);
};
  return 2.0 * $sum;
};
  function mochi_log10($x) {
  global $PI, $sample_rate, $size, $audio, $n, $t, $coeffs;
  return ln($x) / ln(10.0);
};
  function sqrtApprox($x) {
  global $PI, $sample_rate, $size, $audio, $n, $t, $coeffs;
  if ($x <= 0.0) {
  return 0.0;
}
  $guess = $x;
  $i = 0;
  while ($i < 10) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = _iadd($i, 1);
};
  return $guess;
};
  function absf($x) {
  global $PI, $sample_rate, $size, $audio, $n, $t, $coeffs;
  if ($x < 0.0) {
  return -$x;
}
  return $x;
};
  function normalize($audio) {
  global $PI, $sample_rate, $size, $n, $t, $coeffs;
  $max_val = 0.0;
  $i = 0;
  while ($i < count($audio)) {
  $v = absf($audio[$i]);
  if ($v > $max_val) {
  $max_val = $v;
}
  $i = _iadd($i, 1);
};
  $res = [];
  $i = 0;
  while ($i < count($audio)) {
  $res = _append($res, $audio[$i] / $max_val);
  $i = _iadd($i, 1);
};
  return $res;
};
  function dft($frame, $bins) {
  global $PI, $sample_rate, $size, $audio, $t, $coeffs;
  $N = count($frame);
  $spec = [];
  $k = 0;
  while ($k < $bins) {
  $real = 0.0;
  $imag = 0.0;
  $n = 0;
  while ($n < $N) {
  $angle = -2.0 * $PI * (floatval($k)) * (floatval($n)) / (floatval($N));
  $real = $real + $frame[$n] * cosApprox($angle);
  $imag = $imag + $frame[$n] * sinApprox($angle);
  $n = _iadd($n, 1);
};
  $spec = _append($spec, $real * $real + $imag * $imag);
  $k = _iadd($k, 1);
};
  return $spec;
};
  function triangular_filters($bins, $spectrum_size) {
  global $PI, $sample_rate, $size, $audio, $n, $t, $coeffs;
  $filters = [];
  $b = 0;
  while ($b < $bins) {
  $center = _intdiv((_imul((_iadd($b, 1)), $spectrum_size)), (_iadd($bins, 1)));
  $filt = [];
  $i = 0;
  while ($i < $spectrum_size) {
  $v = 0.0;
  if ($i <= $center) {
  $v = (floatval($i)) / (floatval($center));
} else {
  $v = (floatval((_isub($spectrum_size, $i)))) / (floatval((_isub($spectrum_size, $center))));
}
  $filt = _append($filt, $v);
  $i = _iadd($i, 1);
};
  $filters = _append($filters, $filt);
  $b = _iadd($b, 1);
};
  return $filters;
};
  function dot($mat, $vec) {
  global $PI, $sample_rate, $size, $audio, $n, $t, $coeffs;
  $res = [];
  $i = 0;
  while ($i < count($mat)) {
  $sum = 0.0;
  $j = 0;
  while ($j < count($vec)) {
  $sum = $sum + $mat[$i][$j] * $vec[$j];
  $j = _iadd($j, 1);
};
  $res = _append($res, $sum);
  $i = _iadd($i, 1);
};
  return $res;
};
  function discrete_cosine_transform($dct_filter_num, $filter_num) {
  global $PI, $sample_rate, $size, $audio, $n, $t, $coeffs;
  $basis = [];
  $i = 0;
  while ($i < $dct_filter_num) {
  $row = [];
  $j = 0;
  while ($j < $filter_num) {
  if ($i == 0) {
  $row = _append($row, 1.0 / sqrtApprox(floatval($filter_num)));
} else {
  $angle = (floatval((_iadd(_imul(2, $j), 1)))) * (floatval($i)) * $PI / (2.0 * (floatval($filter_num)));
  $row = _append($row, cosApprox($angle) * sqrtApprox(2.0 / (floatval($filter_num))));
}
  $j = _iadd($j, 1);
};
  $basis = _append($basis, $row);
  $i = _iadd($i, 1);
};
  return $basis;
};
  function mfcc($audio, $bins, $dct_num) {
  global $PI, $sample_rate, $size, $n, $t, $coeffs;
  $norm = normalize($audio);
  $spec = dft($norm, _iadd($bins, 2));
  $filters = triangular_filters($bins, count($spec));
  $energies = dot($filters, $spec);
  $logfb = [];
  $i = 0;
  while ($i < count($energies)) {
  $logfb = _append($logfb, 10.0 * mochi_log10($energies[$i] + 0.0000000001));
  $i = _iadd($i, 1);
};
  $dct_basis = discrete_cosine_transform($dct_num, $bins);
  $res = dot($dct_basis, $logfb);
  if (count($res) == 0) {
  $res = [0.0, 0.0, 0.0];
}
  return $res;
};
  $sample_rate = 8000;
  $size = 16;
  $audio = [];
  $n = 0;
  while ($n < $size) {
  $t = (floatval($n)) / (floatval($sample_rate));
  $audio = _append($audio, sinApprox(2.0 * $PI * 440.0 * $t));
  $n = _iadd($n, 1);
}
  $coeffs = mfcc($audio, 5, 3);
  foreach ($coeffs as $c) {
  echo rtrim(json_encode($c, 1344)), PHP_EOL;
}
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
