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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function to_float($x) {
  return $x * 1.0;
};
  function ln($x) {
  if ($x <= 0.0) {
  _panic('ln domain error');
}
  $y = ($x - 1.0) / ($x + 1.0);
  $y2 = $y * $y;
  $term = $y;
  $sum = 0.0;
  $k = 0;
  while ($k < 10) {
  $denom = to_float(2 * $k + 1);
  $sum = $sum + $term / $denom;
  $term = $term * $y2;
  $k = $k + 1;
};
  return 2.0 * $sum;
};
  function mochi_exp($x) {
  $term = 1.0;
  $sum = 1.0;
  $n = 1;
  while ($n < 20) {
  $term = $term * $x / to_float($n);
  $sum = $sum + $term;
  $n = $n + 1;
};
  return $sum;
};
  function pow_float($base, $exponent) {
  return mochi_exp($exponent * ln($base));
};
  function get_altitude_at_pressure($pressure) {
  if ($pressure > 101325.0) {
  _panic('Value Higher than Pressure at Sea Level !');
}
  if ($pressure < 0.0) {
  _panic('Atmospheric Pressure can not be negative !');
}
  $ratio = $pressure / 101325.0;
  return 44330.0 * (1.0 - pow_float($ratio, 1.0 / 5.5255));
};
  echo rtrim(_str(get_altitude_at_pressure(100000.0))), PHP_EOL;
  echo rtrim(_str(get_altitude_at_pressure(101325.0))), PHP_EOL;
  echo rtrim(_str(get_altitude_at_pressure(80000.0))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
