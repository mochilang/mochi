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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
function exp_approx($x) {
  global $r1, $r2, $r3;
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
  $n = $n + 1;
};
  if ($is_neg) {
  return 1.0 / $sum;
}
  return $sum;
}
function ln_series($x) {
  global $r1, $r2, $r3;
  $t = ($x - 1.0) / ($x + 1.0);
  $term = $t;
  $sum = 0.0;
  $n = 1;
  while ($n <= 19) {
  $sum = $sum + $term / (floatval($n));
  $term = $term * $t * $t;
  $n = $n + 2;
};
  return 2.0 * $sum;
}
function ln($x) {
  global $r1, $r2, $r3;
  $y = $x;
  $k = 0;
  while ($y >= 10.0) {
  $y = $y / 10.0;
  $k = $k + 1;
};
  while ($y < 1.0) {
  $y = $y * 10.0;
  $k = $k - 1;
};
  return ln_series($y) + (floatval($k)) * ln_series(10.0);
}
function powf($base, $exponent) {
  global $r1, $r2, $r3;
  return exp_approx($exponent * ln($base));
}
function rainfall_intensity($coefficient_k, $coefficient_a, $coefficient_b, $coefficient_c, $return_period, $duration) {
  global $r1, $r2, $r3;
  if ($coefficient_k <= 0.0) {
  _panic('All parameters must be positive.');
}
  if ($coefficient_a <= 0.0) {
  _panic('All parameters must be positive.');
}
  if ($coefficient_b <= 0.0) {
  _panic('All parameters must be positive.');
}
  if ($coefficient_c <= 0.0) {
  _panic('All parameters must be positive.');
}
  if ($return_period <= 0.0) {
  _panic('All parameters must be positive.');
}
  if ($duration <= 0.0) {
  _panic('All parameters must be positive.');
}
  $numerator = $coefficient_k * powf($return_period, $coefficient_a);
  $denominator = powf($duration + $coefficient_b, $coefficient_c);
  return $numerator / $denominator;
}
$r1 = rainfall_intensity(1000.0, 0.2, 11.6, 0.81, 10.0, 60.0);
echo rtrim(_str($r1)), PHP_EOL;
$r2 = rainfall_intensity(1000.0, 0.2, 11.6, 0.81, 10.0, 30.0);
echo rtrim(_str($r2)), PHP_EOL;
$r3 = rainfall_intensity(1000.0, 0.2, 11.6, 0.81, 5.0, 60.0);
echo rtrim(_str($r3)), PHP_EOL;
