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
$__start_mem = memory_get_usage();
$__start = _now();
  function absf($x) {
  global $coefficient, $constant, $init_val, $iterations, $result;
  return ($x < 0.0 ? -$x : $x);
};
  function strictly_diagonally_dominant($matrix) {
  global $coefficient, $constant, $init_val, $iterations, $result;
  $n = count($matrix);
  $i = 0;
  while ($i < $n) {
  $sum = 0.0;
  $j = 0;
  while ($j < $n) {
  if ($i != $j) {
  $sum = $sum + absf($matrix[$i][$j]);
}
  $j = $j + 1;
};
  if (absf($matrix[$i][$i]) <= $sum) {
  $panic('Coefficient matrix is not strictly diagonally dominant');
}
  $i = $i + 1;
};
  return true;
};
  function jacobi_iteration_method($coefficient, $constant, $init_val, $iterations) {
  global $result;
  $n = count($coefficient);
  if ($n == 0) {
  $panic('Coefficient matrix cannot be empty');
}
  if (count($constant) != $n) {
  $panic('Constant vector length must equal number of rows in coefficient matrix');
}
  if (count($init_val) != $n) {
  $panic('Initial values count must match matrix size');
}
  $r = 0;
  while ($r < $n) {
  if (count($coefficient[$r]) != $n) {
  $panic('Coefficient matrix must be square');
}
  $r = $r + 1;
};
  if ($iterations <= 0) {
  $panic('Iterations must be at least 1');
}
  strictly_diagonally_dominant($coefficient);
  $x = $init_val;
  $k = 0;
  while ($k < $iterations) {
  $new_x = [];
  $i = 0;
  while ($i < $n) {
  $sum = 0.0;
  $j = 0;
  while ($j < $n) {
  if ($i != $j) {
  $sum = $sum + $coefficient[$i][$j] * $x[$j];
}
  $j = $j + 1;
};
  $value = ($constant[$i] - $sum) / $coefficient[$i][$i];
  $new_x = _append($new_x, $value);
  $i = $i + 1;
};
  $x = $new_x;
  $k = $k + 1;
};
  return $x;
};
  $coefficient = [[4.0, 1.0, 1.0], [1.0, 5.0, 2.0], [1.0, 2.0, 4.0]];
  $constant = [2.0, -6.0, -4.0];
  $init_val = [0.5, -0.5, -0.5];
  $iterations = 3;
  $result = jacobi_iteration_method($coefficient, $constant, $init_val, $iterations);
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($result, 1344)))))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
