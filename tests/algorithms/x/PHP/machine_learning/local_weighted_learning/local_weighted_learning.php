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
  function expApprox($x) {
  global $x_train, $y_train, $preds;
  if ($x < 0.0) {
  return 1.0 / expApprox(-$x);
}
  if ($x > 1.0) {
  $half = expApprox($x / 2.0);
  return $half * $half;
}
  $sum = 1.0;
  $term = 1.0;
  $n = 1;
  while ($n < 20) {
  $term = $term * $x / (floatval($n));
  $sum = $sum + $term;
  $n = $n + 1;
};
  return $sum;
};
  function transpose($mat) {
  global $x_train, $y_train, $preds;
  $rows = count($mat);
  $cols = count($mat[0]);
  $res = [];
  $i = 0;
  while ($i < $cols) {
  $row = [];
  $j = 0;
  while ($j < $rows) {
  $row = _append($row, $mat[$j][$i]);
  $j = $j + 1;
};
  $res = _append($res, $row);
  $i = $i + 1;
};
  return $res;
};
  function matMul($a, $b) {
  global $x_train, $y_train, $preds;
  $a_rows = count($a);
  $a_cols = count($a[0]);
  $b_cols = count($b[0]);
  $res = [];
  $i = 0;
  while ($i < $a_rows) {
  $row = [];
  $j = 0;
  while ($j < $b_cols) {
  $sum = 0.0;
  $k = 0;
  while ($k < $a_cols) {
  $sum = $sum + $a[$i][$k] * $b[$k][$j];
  $k = $k + 1;
};
  $row = _append($row, $sum);
  $j = $j + 1;
};
  $res = _append($res, $row);
  $i = $i + 1;
};
  return $res;
};
  function matInv($mat) {
  global $x_train, $y_train, $preds;
  $n = count($mat);
  $aug = [];
  $i = 0;
  while ($i < $n) {
  $row = [];
  $j = 0;
  while ($j < $n) {
  $row = _append($row, $mat[$i][$j]);
  $j = $j + 1;
};
  $j = 0;
  while ($j < $n) {
  if ($i == $j) {
  $row = _append($row, 1.0);
} else {
  $row = _append($row, 0.0);
}
  $j = $j + 1;
};
  $aug = _append($aug, $row);
  $i = $i + 1;
};
  $col = 0;
  while ($col < $n) {
  $pivot = $aug[$col][$col];
  if ($pivot == 0.0) {
  $panic('Matrix is singular');
}
  $j = 0;
  while ($j < 2 * $n) {
  $aug[$col][$j] = $aug[$col][$j] / $pivot;
  $j = $j + 1;
};
  $r = 0;
  while ($r < $n) {
  if ($r != $col) {
  $factor = $aug[$r][$col];
  $j = 0;
  while ($j < 2 * $n) {
  $aug[$r][$j] = $aug[$r][$j] - $factor * $aug[$col][$j];
  $j = $j + 1;
};
}
  $r = $r + 1;
};
  $col = $col + 1;
};
  $inv = [];
  $i = 0;
  while ($i < $n) {
  $row = [];
  $j = 0;
  while ($j < $n) {
  $row = _append($row, $aug[$i][$j + $n]);
  $j = $j + 1;
};
  $inv = _append($inv, $row);
  $i = $i + 1;
};
  return $inv;
};
  function weight_matrix($point, $x_train, $tau) {
  global $y_train, $preds;
  $m = count($x_train);
  $weights = [];
  $i = 0;
  while ($i < $m) {
  $row = [];
  $j = 0;
  while ($j < $m) {
  if ($i == $j) {
  $row = _append($row, 1.0);
} else {
  $row = _append($row, 0.0);
}
  $j = $j + 1;
};
  $weights = _append($weights, $row);
  $i = $i + 1;
};
  $j = 0;
  while ($j < $m) {
  $diff_sq = 0.0;
  $k = 0;
  while ($k < count($point)) {
  $diff = $point[$k] - $x_train[$j][$k];
  $diff_sq = $diff_sq + $diff * $diff;
  $k = $k + 1;
};
  $weights[$j][$j] = expApprox(-$diff_sq / (2.0 * $tau * $tau));
  $j = $j + 1;
};
  return $weights;
};
  function local_weight($point, $x_train, $y_train, $tau) {
  global $preds;
  $w = weight_matrix($point, $x_train, $tau);
  $x_t = transpose($x_train);
  $x_t_w = matMul($x_t, $w);
  $x_t_w_x = matMul($x_t_w, $x_train);
  $inv_part = matInv($x_t_w_x);
  $y_col = [];
  $i = 0;
  while ($i < count($y_train)) {
  $y_col = _append($y_col, [$y_train[$i]]);
  $i = $i + 1;
};
  $x_t_w_y = matMul($x_t_w, $y_col);
  return matMul($inv_part, $x_t_w_y);
};
  function local_weight_regression($x_train, $y_train, $tau) {
  $m = count($x_train);
  $preds = [];
  $i = 0;
  while ($i < $m) {
  $theta = local_weight($x_train[$i], $x_train, $y_train, $tau);
  $weights_vec = [];
  $k = 0;
  while ($k < count($theta)) {
  $weights_vec = _append($weights_vec, $theta[$k][0]);
  $k = $k + 1;
};
  $pred = 0.0;
  $j = 0;
  while ($j < count($x_train[$i])) {
  $pred = $pred + $x_train[$i][$j] * $weights_vec[$j];
  $j = $j + 1;
};
  $preds = _append($preds, $pred);
  $i = $i + 1;
};
  return $preds;
};
  $x_train = [[16.99, 10.34], [21.01, 23.68], [24.59, 25.69]];
  $y_train = [1.01, 1.66, 3.5];
  $preds = local_weight_regression($x_train, $y_train, 0.6);
  echo str_replace('    ', '  ', json_encode($preds, 128)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
