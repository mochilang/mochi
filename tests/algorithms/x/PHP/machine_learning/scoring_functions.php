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
$__start_mem = memory_get_usage();
$__start = _now();
  function absf($x) {
  if ($x < 0.0) {
  return 0.0 - $x;
}
  return $x;
};
  function sqrtApprox($x) {
  if ($x <= 0.0) {
  return 0.0;
}
  $guess = $x;
  $i = 0;
  while ($i < 20) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
};
  function ln_series($x) {
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
  function mae($predict, $actual) {
  $sum = 0.0;
  $i = 0;
  while ($i < count($predict)) {
  $diff = $predict[$i] - $actual[$i];
  $sum = $sum + absf($diff);
  $i = $i + 1;
};
  return $sum / (floatval(count($predict)));
};
  function mse($predict, $actual) {
  $sum = 0.0;
  $i = 0;
  while ($i < count($predict)) {
  $diff = $predict[$i] - $actual[$i];
  $sum = $sum + $diff * $diff;
  $i = $i + 1;
};
  return $sum / (floatval(count($predict)));
};
  function rmse($predict, $actual) {
  return sqrtApprox(mse($predict, $actual));
};
  function rmsle($predict, $actual) {
  $sum = 0.0;
  $i = 0;
  while ($i < count($predict)) {
  $lp = ln($predict[$i] + 1.0);
  $la = ln($actual[$i] + 1.0);
  $diff = $lp - $la;
  $sum = $sum + $diff * $diff;
  $i = $i + 1;
};
  return sqrtApprox($sum / (floatval(count($predict))));
};
  function mbd($predict, $actual) {
  $diff_sum = 0.0;
  $actual_sum = 0.0;
  $i = 0;
  while ($i < count($predict)) {
  $diff_sum = $diff_sum + ($predict[$i] - $actual[$i]);
  $actual_sum = $actual_sum + $actual[$i];
  $i = $i + 1;
};
  $n = floatval(count($predict));
  $numerator = $diff_sum / $n;
  $denominator = $actual_sum / $n;
  return $numerator / $denominator * 100.0;
};
  function manual_accuracy($predict, $actual) {
  $correct = 0;
  $i = 0;
  while ($i < count($predict)) {
  if ($predict[$i] == $actual[$i]) {
  $correct = $correct + 1;
}
  $i = $i + 1;
};
  return (floatval($correct)) / (floatval(count($predict)));
};
  function main() {
  $actual = [1.0, 2.0, 3.0];
  $predict = [1.0, 4.0, 3.0];
  echo rtrim(_str(mae($predict, $actual))), PHP_EOL;
  echo rtrim(_str(mse($predict, $actual))), PHP_EOL;
  echo rtrim(_str(rmse($predict, $actual))), PHP_EOL;
  echo rtrim(_str(rmsle([10.0, 2.0, 30.0], [10.0, 10.0, 30.0]))), PHP_EOL;
  echo rtrim(_str(mbd([2.0, 3.0, 4.0], [1.0, 2.0, 3.0]))), PHP_EOL;
  echo rtrim(_str(mbd([0.0, 1.0, 1.0], [1.0, 2.0, 3.0]))), PHP_EOL;
  echo rtrim(_str(manual_accuracy($predict, $actual))), PHP_EOL;
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
