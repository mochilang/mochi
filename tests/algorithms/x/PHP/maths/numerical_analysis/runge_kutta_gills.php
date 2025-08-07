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
$__start_mem = memory_get_usage();
$__start = _now();
  function sqrt($x) {
  global $y1, $y2;
  $guess = ($x > 1.0 ? $x / 2.0 : 1.0);
  $i = 0;
  while ($i < 20) {
  $guess = 0.5 * ($guess + $x / $guess);
  $i = $i + 1;
};
  return $guess;
};
  function runge_kutta_gills($func, $x_initial, $y_initial, $step_size, $x_final) {
  global $y1, $y2;
  if ($x_initial >= $x_final) {
  $panic('The final value of x must be greater than initial value of x.');
}
  if ($step_size <= 0.0) {
  $panic('Step size must be positive.');
}
  $n = intval((($x_final - $x_initial) / $step_size));
  $y = [];
  $i = 0;
  while ($i <= $n) {
  $y = _append($y, 0.0);
  $i = $i + 1;
};
  $y[0] = $y_initial;
  $xi = $x_initial;
  $idx = 0;
  $root2 = sqrt(2.0);
  while ($idx < $n) {
  $k1 = $step_size * $func($xi, $y[$idx]);
  $k2 = $step_size * $func($xi + $step_size / 2.0, $y[$idx] + $k1 / 2.0);
  $k3 = $step_size * $func($xi + $step_size / 2.0, $y[$idx] + (-0.5 + 1.0 / $root2) * $k1 + (1.0 - 1.0 / $root2) * $k2);
  $k4 = $step_size * $func($xi + $step_size, $y[$idx] - (1.0 / $root2) * $k2 + (1.0 + 1.0 / $root2) * $k3);
  $y[$idx + 1] = $y[$idx] + ($k1 + (2.0 - $root2) * $k2 + (2.0 + $root2) * $k3 + $k4) / 6.0;
  $xi = $xi + $step_size;
  $idx = $idx + 1;
};
  return $y;
};
  function f1($x, $y) {
  global $y1, $y2;
  return ($x - $y) / 2.0;
};
  $y1 = runge_kutta_gills('f1', 0.0, 3.0, 0.2, 5.0);
  echo rtrim(_str($y1[count($y1) - 1])), PHP_EOL;
  function f2($x, $y) {
  global $y1, $y2;
  return $x;
};
  $y2 = runge_kutta_gills('f2', -1.0, 0.0, 0.2, 0.0);
  echo rtrim(_str($y2)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
