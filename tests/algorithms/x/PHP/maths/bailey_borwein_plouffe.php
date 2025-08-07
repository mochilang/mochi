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
  function mod_pow($base, $exponent, $modulus) {
  global $digits, $i;
  $result = 1;
  $b = $base % $modulus;
  $e = $exponent;
  while ($e > 0) {
  if ($e % 2 == 1) {
  $result = ($result * $b) % $modulus;
}
  $b = ($b * $b) % $modulus;
  $e = _intdiv($e, 2);
};
  return $result;
};
  function pow_float($base, $exponent) {
  global $digits;
  $exp = $exponent;
  $result = 1.0;
  if ($exp < 0) {
  $exp = -$exp;
}
  $i = 0;
  while ($i < $exp) {
  $result = $result * $base;
  $i = $i + 1;
};
  if ($exponent < 0) {
  $result = 1.0 / $result;
}
  return $result;
};
  function hex_digit($n) {
  global $digits, $i;
  if ($n < 10) {
  return _str($n);
}
  $letters = ['a', 'b', 'c', 'd', 'e', 'f'];
  return $letters[$n - 10];
};
  function floor_float($x) {
  global $digits;
  $i = intval($x);
  if ((floatval($i)) > $x) {
  $i = $i - 1;
}
  return floatval($i);
};
  function subsum($digit_pos_to_extract, $denominator_addend, $precision) {
  global $digits, $i;
  $total = 0.0;
  $sum_index = 0;
  while ($sum_index < $digit_pos_to_extract + $precision) {
  $denominator = 8 * $sum_index + $denominator_addend;
  if ($sum_index < $digit_pos_to_extract) {
  $exponent = $digit_pos_to_extract - 1 - $sum_index;
  $exponential_term = mod_pow(16, $exponent, $denominator);
  $total = $total + (floatval($exponential_term)) / (floatval($denominator));
} else {
  $exponent = $digit_pos_to_extract - 1 - $sum_index;
  $exponential_term = pow_float(16.0, $exponent);
  $total = $total + $exponential_term / (floatval($denominator));
}
  $sum_index = $sum_index + 1;
};
  return $total;
};
  function bailey_borwein_plouffe($digit_position, $precision) {
  global $digits, $i;
  if ($digit_position <= 0) {
  $panic('Digit position must be a positive integer');
}
  if ($precision < 0) {
  $panic('Precision must be a nonnegative integer');
}
  $sum_result = 4.0 * subsum($digit_position, 1, $precision) - 2.0 * subsum($digit_position, 4, $precision) - 1.0 * subsum($digit_position, 5, $precision) - 1.0 * subsum($digit_position, 6, $precision);
  $fraction = $sum_result - floor_float($sum_result);
  $digit = intval(($fraction * 16.0));
  $hd = hex_digit($digit);
  return $hd;
};
  $digits = '';
  $i = 1;
  while ($i <= 10) {
  $digits = $digits . bailey_borwein_plouffe($i, 1000);
  $i = $i + 1;
}
  echo rtrim($digits), PHP_EOL;
  echo rtrim(bailey_borwein_plouffe(5, 10000)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
