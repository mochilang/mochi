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
  function mochi_key($x, $y) {
  return _str($x) . ',' . _str($y);
};
  function joint_probability_distribution($x_values, $y_values, $x_probabilities, $y_probabilities) {
  $result = [];
  $i = 0;
  while ($i < count($x_values)) {
  $j = 0;
  while ($j < count($y_values)) {
  $k = mochi_key($x_values[$i], $y_values[$j]);
  $result[$k] = $x_probabilities[$i] * $y_probabilities[$j];
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $result;
};
  function expectation($values, $probabilities) {
  $total = 0.0;
  $i = 0;
  while ($i < count($values)) {
  $total = $total + (floatval($values[$i])) * $probabilities[$i];
  $i = $i + 1;
};
  return $total;
};
  function variance($values, $probabilities) {
  $mean = expectation($values, $probabilities);
  $total = 0.0;
  $i = 0;
  while ($i < count($values)) {
  $diff = (floatval($values[$i])) - $mean;
  $total = $total + $diff * $diff * $probabilities[$i];
  $i = $i + 1;
};
  return $total;
};
  function covariance($x_values, $y_values, $x_probabilities, $y_probabilities) {
  $mean_x = expectation($x_values, $x_probabilities);
  $mean_y = expectation($y_values, $y_probabilities);
  $total = 0.0;
  $i = 0;
  while ($i < count($x_values)) {
  $j = 0;
  while ($j < count($y_values)) {
  $diff_x = (floatval($x_values[$i])) - $mean_x;
  $diff_y = (floatval($y_values[$j])) - $mean_y;
  $total = $total + $diff_x * $diff_y * $x_probabilities[$i] * $y_probabilities[$j];
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $total;
};
  function sqrtApprox($x) {
  if ($x <= 0.0) {
  return 0.0;
}
  $guess = $x / 2.0;
  $i = 0;
  while ($i < 20) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
};
  function standard_deviation($v) {
  return sqrtApprox($v);
};
  function main() {
  $x_values = [1, 2];
  $y_values = [-2, 5, 8];
  $x_probabilities = [0.7, 0.3];
  $y_probabilities = [0.3, 0.5, 0.2];
  $jpd = joint_probability_distribution($x_values, $y_values, $x_probabilities, $y_probabilities);
  $i = 0;
  while ($i < count($x_values)) {
  $j = 0;
  while ($j < count($y_values)) {
  $k = mochi_key($x_values[$i], $y_values[$j]);
  $prob = $jpd[$k];
  echo rtrim($k . '=' . _str($prob)), PHP_EOL;
  $j = $j + 1;
};
  $i = $i + 1;
};
  $ex = expectation($x_values, $x_probabilities);
  $ey = expectation($y_values, $y_probabilities);
  $vx = variance($x_values, $x_probabilities);
  $vy = variance($y_values, $y_probabilities);
  $cov = covariance($x_values, $y_values, $x_probabilities, $y_probabilities);
  echo rtrim('Ex=' . _str($ex)), PHP_EOL;
  echo rtrim('Ey=' . _str($ey)), PHP_EOL;
  echo rtrim('Vx=' . _str($vx)), PHP_EOL;
  echo rtrim('Vy=' . _str($vy)), PHP_EOL;
  echo rtrim('Cov=' . _str($cov)), PHP_EOL;
  echo rtrim('Sx=' . _str(standard_deviation($vx))), PHP_EOL;
  echo rtrim('Sy=' . _str(standard_deviation($vy))), PHP_EOL;
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
