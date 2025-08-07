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
$__start_mem = memory_get_usage();
$__start = _now();
  function c_add($a, $b) {
  global $PI, $A, $B, $product;
  return ['re' => $a['re'] + $b['re'], 'im' => $a['im'] + $b['im']];
};
  function c_sub($a, $b) {
  global $PI, $A, $B, $product;
  return ['re' => $a['re'] - $b['re'], 'im' => $a['im'] - $b['im']];
};
  function c_mul($a, $b) {
  global $PI, $A, $B, $product;
  return ['re' => $a['re'] * $b['re'] - $a['im'] * $b['im'], 'im' => $a['re'] * $b['im'] + $a['im'] * $b['re']];
};
  function c_mul_scalar($a, $s) {
  global $PI, $A, $B, $product;
  return ['re' => $a['re'] * $s, 'im' => $a['im'] * $s];
};
  function c_div_scalar($a, $s) {
  global $PI, $A, $B, $product;
  return ['re' => $a['re'] / $s, 'im' => $a['im'] / $s];
};
  $PI = 3.141592653589793;
  function sin_taylor($x) {
  global $PI, $A, $B, $product;
  $term = $x;
  $sum = $x;
  $i = 1;
  while ($i < 10) {
  $k1 = 2.0 * (floatval($i));
  $k2 = $k1 + 1.0;
  $term = -$term * $x * $x / ($k1 * $k2);
  $sum = $sum + $term;
  $i = $i + 1;
};
  return $sum;
};
  function cos_taylor($x) {
  global $PI, $A, $B, $product;
  $term = 1.0;
  $sum = 1.0;
  $i = 1;
  while ($i < 10) {
  $k1 = 2.0 * (floatval($i)) - 1.0;
  $k2 = 2.0 * (floatval($i));
  $term = -$term * $x * $x / ($k1 * $k2);
  $sum = $sum + $term;
  $i = $i + 1;
};
  return $sum;
};
  function exp_i($theta) {
  global $PI, $A, $B, $product;
  return ['re' => cos_taylor($theta), 'im' => sin_taylor($theta)];
};
  function make_complex_list($n, $value) {
  global $PI, $A, $B, $product;
  $arr = [];
  $i = 0;
  while ($i < $n) {
  $arr = _append($arr, $value);
  $i = $i + 1;
};
  return $arr;
};
  function fft($a, $invert) {
  global $PI, $A, $B, $product;
  $n = count($a);
  if ($n == 1) {
  return [$a[0]];
}
  $a0 = [];
  $a1 = [];
  $i = 0;
  while ($i < _intdiv($n, 2)) {
  $a0 = _append($a0, $a[2 * $i]);
  $a1 = _append($a1, $a[2 * $i + 1]);
  $i = $i + 1;
};
  $y0 = fft($a0, $invert);
  $y1 = fft($a1, $invert);
  $angle = 2.0 * $PI / (floatval($n)) * (($invert ? -1.0 : 1.0));
  $w = ['re' => 1.0, 'im' => 0.0];
  $wn = exp_i($angle);
  $y = make_complex_list($n, ['re' => 0.0, 'im' => 0.0]);
  $i = 0;
  while ($i < _intdiv($n, 2)) {
  $t = c_mul($w, $y1[$i]);
  $u = $y0[$i];
  $even = c_add($u, $t);
  $odd = c_sub($u, $t);
  if ($invert) {
  $even = c_div_scalar($even, 2.0);
  $odd = c_div_scalar($odd, 2.0);
}
  $y[$i] = $even;
  $y[$i + _intdiv($n, 2)] = $odd;
  $w = c_mul($w, $wn);
  $i = $i + 1;
};
  return $y;
};
  function mochi_floor($x) {
  global $PI, $A, $B, $product;
  $i = intval($x);
  if ((floatval($i)) > $x) {
  $i = $i - 1;
}
  return floatval($i);
};
  function pow10($n) {
  global $PI, $A, $B, $product;
  $p = 1.0;
  $i = 0;
  while ($i < $n) {
  $p = $p * 10.0;
  $i = $i + 1;
};
  return $p;
};
  function round_to($x, $ndigits) {
  global $PI, $A, $B, $product;
  $m = pow10($ndigits);
  return mochi_floor($x * $m + 0.5) / $m;
};
  function list_to_string($l) {
  global $PI, $A, $B, $product;
  $s = '[';
  $i = 0;
  while ($i < count($l)) {
  $s = $s . _str($l[$i]);
  if ($i + 1 < count($l)) {
  $s = $s . ', ';
}
  $i = $i + 1;
};
  $s = $s . ']';
  return $s;
};
  function multiply_poly($a, $b) {
  global $PI, $A, $B, $product;
  $n = 1;
  while ($n < count($a) + count($b) - 1) {
  $n = $n * 2;
};
  $fa = make_complex_list($n, ['re' => 0.0, 'im' => 0.0]);
  $fb = make_complex_list($n, ['re' => 0.0, 'im' => 0.0]);
  $i = 0;
  while ($i < count($a)) {
  $fa[$i] = ['re' => $a[$i], 'im' => 0.0];
  $i = $i + 1;
};
  $i = 0;
  while ($i < count($b)) {
  $fb[$i] = ['re' => $b[$i], 'im' => 0.0];
  $i = $i + 1;
};
  $fa = fft($fa, false);
  $fb = fft($fb, false);
  $i = 0;
  while ($i < $n) {
  $fa[$i] = c_mul($fa[$i], $fb[$i]);
  $i = $i + 1;
};
  $fa = fft($fa, true);
  $res = [];
  $i = 0;
  while ($i < count($a) + count($b) - 1) {
  $val = $fa[$i];
  $res = _append($res, round_to($val['re'], 8));
  $i = $i + 1;
};
  while (count($res) > 0 && $res[count($res) - 1] == 0.0) {
  $res = array_slice($res, 0, count($res) - 1 - 0);
};
  return $res;
};
  $A = [0.0, 1.0, 0.0, 2.0];
  $B = [2.0, 3.0, 4.0, 0.0];
  $product = multiply_poly($A, $B);
  echo rtrim(list_to_string($product)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
