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
$__start_mem = memory_get_usage();
$__start = _now();
  function exp_approx($x) {
  $neg = false;
  $y = $x;
  if ($x < 0.0) {
  $neg = true;
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
  if ($neg) {
  return 1.0 / $sum;
}
  return $sum;
};
  function ln_series($x) {
  $t = ($x - 1.0) / ($x + 1.0);
  $term = $t;
  $acc = 0.0;
  $n = 1;
  while ($n <= 19) {
  $acc = $acc + $term / (floatval($n));
  $term = $term * $t * $t;
  $n = $n + 2;
};
  return 2.0 * $acc;
};
  function ln($x) {
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
};
  function softplus($x) {
  return ln(1.0 + exp_approx($x));
};
  function tanh_approx($x) {
  return (2.0 / (1.0 + exp_approx(-2.0 * $x))) - 1.0;
};
  function mish($vector) {
  $result = [];
  $i = 0;
  while ($i < count($vector)) {
  $x = $vector[$i];
  $sp = softplus($x);
  $y = $x * tanh_approx($sp);
  $result = _append($result, $y);
  $i = $i + 1;
};
  return $result;
};
  function main() {
  $v1 = [2.3, 0.6, -2.0, -3.8];
  $v2 = [-9.2, -0.3, 0.45, -4.56];
  echo rtrim(_str(mish($v1))), PHP_EOL;
  echo rtrim(_str(mish($v2))), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
