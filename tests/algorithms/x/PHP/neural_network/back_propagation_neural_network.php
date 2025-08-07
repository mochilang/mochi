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
  $seed = 1;
  function mochi_rand() {
  global $seed;
  $seed = ($seed * 1103515245 + 12345) % 2147483648;
  return $seed;
};
  function random() {
  global $seed;
  return (1.0 * mochi_rand()) / 2147483648.0;
};
  function expApprox($x) {
  global $seed;
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
};
  function sigmoid($z) {
  global $seed;
  return 1.0 / (1.0 + expApprox(-$z));
};
  function sigmoid_vec($v) {
  global $seed;
  $res = [];
  $i = 0;
  while ($i < count($v)) {
  $res = _append($res, sigmoid($v[$i]));
  $i = $i + 1;
};
  return $res;
};
  function sigmoid_derivative($out) {
  global $seed;
  $res = [];
  $i = 0;
  while ($i < count($out)) {
  $val = $out[$i];
  $res = _append($res, $val * (1.0 - $val));
  $i = $i + 1;
};
  return $res;
};
  function random_vector($n) {
  global $seed;
  $v = [];
  $i = 0;
  while ($i < $n) {
  $v = _append($v, random() - 0.5);
  $i = $i + 1;
};
  return $v;
};
  function random_matrix($r, $c) {
  global $seed;
  $m = [];
  $i = 0;
  while ($i < $r) {
  $m = _append($m, random_vector($c));
  $i = $i + 1;
};
  return $m;
};
  function matvec($mat, $vec) {
  global $seed;
  $res = [];
  $i = 0;
  while ($i < count($mat)) {
  $s = 0.0;
  $j = 0;
  while ($j < count($vec)) {
  $s = $s + $mat[$i][$j] * $vec[$j];
  $j = $j + 1;
};
  $res = _append($res, $s);
  $i = $i + 1;
};
  return $res;
};
  function matTvec($mat, $vec) {
  global $seed;
  $cols = count($mat[0]);
  $res = [];
  $j = 0;
  while ($j < $cols) {
  $s = 0.0;
  $i = 0;
  while ($i < count($mat)) {
  $s = $s + $mat[$i][$j] * $vec[$i];
  $i = $i + 1;
};
  $res = _append($res, $s);
  $j = $j + 1;
};
  return $res;
};
  function vec_sub($a, $b) {
  global $seed;
  $res = [];
  $i = 0;
  while ($i < count($a)) {
  $res = _append($res, $a[$i] - $b[$i]);
  $i = $i + 1;
};
  return $res;
};
  function vec_mul($a, $b) {
  global $seed;
  $res = [];
  $i = 0;
  while ($i < count($a)) {
  $res = _append($res, $a[$i] * $b[$i]);
  $i = $i + 1;
};
  return $res;
};
  function vec_scalar_mul($v, $s) {
  global $seed;
  $res = [];
  $i = 0;
  while ($i < count($v)) {
  $res = _append($res, $v[$i] * $s);
  $i = $i + 1;
};
  return $res;
};
  function outer($a, $b) {
  global $seed;
  $res = [];
  $i = 0;
  while ($i < count($a)) {
  $row = [];
  $j = 0;
  while ($j < count($b)) {
  $row = _append($row, $a[$i] * $b[$j]);
  $j = $j + 1;
};
  $res = _append($res, $row);
  $i = $i + 1;
};
  return $res;
};
  function mat_scalar_mul($mat, $s) {
  global $seed;
  $res = [];
  $i = 0;
  while ($i < count($mat)) {
  $row = [];
  $j = 0;
  while ($j < count($mat[$i])) {
  $row = _append($row, $mat[$i][$j] * $s);
  $j = $j + 1;
};
  $res = _append($res, $row);
  $i = $i + 1;
};
  return $res;
};
  function mat_sub($a, $b) {
  global $seed;
  $res = [];
  $i = 0;
  while ($i < count($a)) {
  $row = [];
  $j = 0;
  while ($j < count($a[$i])) {
  $row = _append($row, $a[$i][$j] - $b[$i][$j]);
  $j = $j + 1;
};
  $res = _append($res, $row);
  $i = $i + 1;
};
  return $res;
};
  function init_layer($units, $back_units, $lr) {
  global $seed;
  return ['units' => $units, 'weight' => random_matrix($units, $back_units), 'bias' => random_vector($units), 'output' => [], 'xdata' => [], 'learn_rate' => $lr];
};
  function forward(&$layers, $x) {
  global $seed;
  $data = $x;
  $i = 0;
  while ($i < count($layers)) {
  $layer = $layers[$i];
  $layer['xdata'] = $data;
  if ($i == 0) {
  $layer['output'] = $data;
} else {
  $z = vec_sub(matvec($layer['weight'], $data), $layer['bias']);
  $layer['output'] = sigmoid_vec($z);
  $data = $layer['output'];
}
  $layers[$i] = $layer;
  $i = $i + 1;
};
  return $layers;
};
  function backward(&$layers, $grad) {
  global $seed;
  $g = $grad;
  $i = count($layers) - 1;
  while ($i > 0) {
  $layer = $layers[$i];
  $deriv = sigmoid_derivative($layer['output']);
  $delta = vec_mul($g, $deriv);
  $grad_w = outer($delta, $layer['xdata']);
  $layer['weight'] = mat_sub($layer['weight'], mat_scalar_mul($grad_w, $layer['learn_rate']));
  $layer['bias'] = vec_sub($layer['bias'], vec_scalar_mul($delta, $layer['learn_rate']));
  $g = matTvec($layer['weight'], $delta);
  $layers[$i] = $layer;
  $i = $i - 1;
};
  return $layers;
};
  function calc_loss($y, $yhat) {
  global $seed;
  $s = 0.0;
  $i = 0;
  while ($i < count($y)) {
  $d = $y[$i] - $yhat[$i];
  $s = $s + $d * $d;
  $i = $i + 1;
};
  return $s;
};
  function calc_gradient($y, $yhat) {
  global $seed;
  $g = [];
  $i = 0;
  while ($i < count($y)) {
  $g = _append($g, 2.0 * ($yhat[$i] - $y[$i]));
  $i = $i + 1;
};
  return $g;
};
  function train($layers, $xdata, $ydata, $rounds, $acc) {
  global $seed;
  $r = 0;
  while ($r < $rounds) {
  $i = 0;
  while ($i < count($xdata)) {
  $layers = forward($layers, $xdata[$i]);
  $out = $layers[count($layers) - 1]['output'];
  $grad = calc_gradient($ydata[$i], $out);
  $layers = backward($layers, $grad);
  $i = $i + 1;
};
  $r = $r + 1;
};
  return 0.0;
};
  function create_data() {
  global $seed;
  $x = [];
  $i = 0;
  while ($i < 10) {
  $x = _append($x, random_vector(10));
  $i = $i + 1;
};
  $y = [[0.8, 0.4], [0.4, 0.3], [0.34, 0.45], [0.67, 0.32], [0.88, 0.67], [0.78, 0.77], [0.55, 0.66], [0.55, 0.43], [0.54, 0.1], [0.1, 0.5]];
  return ['x' => $x, 'y' => $y];
};
  function main() {
  global $seed;
  $data = create_data();
  $x = $data['x'];
  $y = $data['y'];
  $layers = [];
  $layers = _append($layers, init_layer(10, 0, 0.3));
  $layers = _append($layers, init_layer(20, 10, 0.3));
  $layers = _append($layers, init_layer(30, 20, 0.3));
  $layers = _append($layers, init_layer(2, 30, 0.3));
  $final_mse = train($layers, $x, $y, 100, 0.01);
  echo rtrim(json_encode($final_mse, 1344)), PHP_EOL;
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
