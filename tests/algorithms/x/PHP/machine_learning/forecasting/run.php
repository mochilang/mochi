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
  function int_to_float($x) {
  return $x * 1.0;
};
  function abs_float($x) {
  if ($x < 0.0) {
  return 0.0 - $x;
}
  return $x;
};
  function exp_approx($x) {
  $term = 1.0;
  $sum = 1.0;
  $i = 1;
  while ($i < 10) {
  $term = $term * $x / int_to_float($i);
  $sum = $sum + $term;
  $i = $i + 1;
};
  return $sum;
};
  function floor_int($x) {
  $i = 0;
  while (int_to_float($i + 1) <= $x) {
  $i = $i + 1;
};
  return $i;
};
  function dot($a, $b) {
  $s = 0.0;
  $i = 0;
  while ($i < count($a)) {
  $s = $s + $a[$i] * $b[$i];
  $i = $i + 1;
};
  return $s;
};
  function transpose($m) {
  $rows = count($m);
  $cols = count($m[0]);
  $res = [];
  $j = 0;
  while ($j < $cols) {
  $row = [];
  $i = 0;
  while ($i < $rows) {
  $row = _append($row, $m[$i][$j]);
  $i = $i + 1;
};
  $res = _append($res, $row);
  $j = $j + 1;
};
  return $res;
};
  function matmul($a, $b) {
  $n = count($a);
  $m = count($b[0]);
  $p = count($b);
  $res = [];
  $i = 0;
  while ($i < $n) {
  $row = [];
  $j = 0;
  while ($j < $m) {
  $s = 0.0;
  $k = 0;
  while ($k < $p) {
  $s = $s + $a[$i][$k] * $b[$k][$j];
  $k = $k + 1;
};
  $row = _append($row, $s);
  $j = $j + 1;
};
  $res = _append($res, $row);
  $i = $i + 1;
};
  return $res;
};
  function matvec($a, $b) {
  $res = [];
  $i = 0;
  while ($i < count($a)) {
  $res = _append($res, dot($a[$i], $b));
  $i = $i + 1;
};
  return $res;
};
  function identity($n) {
  $res = [];
  $i = 0;
  while ($i < $n) {
  $row = [];
  $j = 0;
  while ($j < $n) {
  $row = _append($row, ($i == $j ? 1.0 : 0.0));
  $j = $j + 1;
};
  $res = _append($res, $row);
  $i = $i + 1;
};
  return $res;
};
  function invert($mat) {
  $n = count($mat);
  $a = $mat;
  $inv = identity($n);
  $i = 0;
  while ($i < $n) {
  $pivot = $a[$i][$i];
  $j = 0;
  while ($j < $n) {
  $a[$i][$j] = $a[$i][$j] / $pivot;
  $inv[$i][$j] = $inv[$i][$j] / $pivot;
  $j = $j + 1;
};
  $k = 0;
  while ($k < $n) {
  if ($k != $i) {
  $factor = $a[$k][$i];
  $j = 0;
  while ($j < $n) {
  $a[$k][$j] = $a[$k][$j] - $factor * $a[$i][$j];
  $inv[$k][$j] = $inv[$k][$j] - $factor * $inv[$i][$j];
  $j = $j + 1;
};
}
  $k = $k + 1;
};
  $i = $i + 1;
};
  return $inv;
};
  function normal_equation($X, $y) {
  $Xt = transpose($X);
  $XtX = matmul($Xt, $X);
  $XtX_inv = invert($XtX);
  $Xty = matvec($Xt, $y);
  return matvec($XtX_inv, $Xty);
};
  function linear_regression_prediction($train_dt, $train_usr, $train_mtch, $test_dt, $test_mtch) {
  $X = [];
  $i = 0;
  while ($i < count($train_dt)) {
  $X = _append($X, [1.0, $train_dt[$i], $train_mtch[$i]]);
  $i = $i + 1;
};
  $beta = normal_equation($X, $train_usr);
  return abs_float($beta[0] + $test_dt[0] * $beta[1] + $test_mtch[0] * $beta[2]);
};
  function sarimax_predictor($train_user, $train_match, $test_match) {
  $n = count($train_user);
  $X = [];
  $y = [];
  $i = 1;
  while ($i < $n) {
  $X = _append($X, [1.0, $train_user[$i - 1], $train_match[$i]]);
  $y = _append($y, $train_user[$i]);
  $i = $i + 1;
};
  $beta = normal_equation($X, $y);
  return $beta[0] + $beta[1] * $train_user[$n - 1] + $beta[2] * $test_match[0];
};
  function rbf_kernel($a, $b, $gamma) {
  $sum = 0.0;
  $i = 0;
  while ($i < count($a)) {
  $diff = $a[$i] - $b[$i];
  $sum = $sum + $diff * $diff;
  $i = $i + 1;
};
  return exp_approx(-$gamma * $sum);
};
  function support_vector_regressor($x_train, $x_test, $train_user) {
  $gamma = 0.1;
  $weights = [];
  $i = 0;
  while ($i < count($x_train)) {
  $weights = _append($weights, rbf_kernel($x_train[$i], $x_test[0], $gamma));
  $i = $i + 1;
};
  $num = 0.0;
  $den = 0.0;
  $i = 0;
  while ($i < count($train_user)) {
  $num = $num + $weights[$i] * $train_user[$i];
  $den = $den + $weights[$i];
  $i = $i + 1;
};
  return $num / $den;
};
  function set_at_float($xs, $idx, $value) {
  $i = 0;
  $res = [];
  while ($i < count($xs)) {
  if ($i == $idx) {
  $res = _append($res, $value);
} else {
  $res = _append($res, $xs[$i]);
}
  $i = $i + 1;
};
  return $res;
};
  function sort_float($xs) {
  $res = $xs;
  $i = 1;
  while ($i < count($res)) {
  $key = $res[$i];
  $j = $i - 1;
  while ($j >= 0 && $res[$j] > $key) {
  $res = set_at_float($res, $j + 1, $res[$j]);
  $j = $j - 1;
};
  $res = set_at_float($res, $j + 1, $key);
  $i = $i + 1;
};
  return $res;
};
  function percentile($data, $q) {
  $sorted = sort_float($data);
  $n = count($sorted);
  $pos = ($q / 100.0) * int_to_float($n - 1);
  $idx = floor_int($pos);
  $frac = $pos - int_to_float($idx);
  if ($idx + 1 < $n) {
  return $sorted[$idx] * (1.0 - $frac) + $sorted[$idx + 1] * $frac;
}
  return $sorted[$idx];
};
  function interquartile_range_checker($train_user) {
  $q1 = percentile($train_user, 25.0);
  $q3 = percentile($train_user, 75.0);
  $iqr = $q3 - $q1;
  return $q1 - $iqr * 0.1;
};
  function data_safety_checker($list_vote, $actual_result) {
  $safe = 0;
  $not_safe = 0;
  $i = 0;
  while ($i < count($list_vote)) {
  $v = $list_vote[$i];
  if ($v > $actual_result) {
  $safe = $not_safe + 1;
} else {
  if (abs_float(abs_float($v) - abs_float($actual_result)) <= 0.1) {
  $safe = $safe + 1;
} else {
  $not_safe = $not_safe + 1;
};
}
  $i = $i + 1;
};
  return $safe > $not_safe;
};
  function main() {
  $vote = [linear_regression_prediction([2.0, 3.0, 4.0, 5.0], [5.0, 3.0, 4.0, 6.0], [3.0, 1.0, 2.0, 4.0], [2.0], [2.0]), sarimax_predictor([4.0, 2.0, 6.0, 8.0], [3.0, 1.0, 2.0, 4.0], [2.0]), support_vector_regressor([[5.0, 2.0], [1.0, 5.0], [6.0, 2.0]], [[3.0, 2.0]], [2.0, 1.0, 4.0])];
  echo rtrim(json_encode($vote[0], 1344)), PHP_EOL;
  echo rtrim(json_encode($vote[1], 1344)), PHP_EOL;
  echo rtrim(json_encode($vote[2], 1344)), PHP_EOL;
  echo rtrim(json_encode(data_safety_checker($vote, 5.0), 1344)), PHP_EOL;
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
