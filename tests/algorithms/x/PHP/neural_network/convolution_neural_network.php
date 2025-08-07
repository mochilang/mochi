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
function _len($x) {
    if ($x === null) { return 0; }
    if (is_array($x)) { return count($x); }
    if (is_string($x)) { return strlen($x); }
    return strlen(strval($x));
}
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  $seed = 1;
  function random() {
  global $seed;
  $seed = ($seed * 13 + 7) % 100;
  return (floatval($seed)) / 100.0;
};
  function sigmoid($x) {
  global $seed;
  return 1.0 / (1.0 + mochi_exp(-$x));
};
  function to_float($x) {
  global $seed;
  return $x * 1.0;
};
  function mochi_exp($x) {
  global $seed;
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
  function convolve($data, $kernel, $step, $bias) {
  global $seed;
  $size_data = count($data);
  $size_kernel = count($kernel);
  $out = [];
  $i = 0;
  while ($i <= $size_data - $size_kernel) {
  $row = [];
  $j = 0;
  while ($j <= $size_data - $size_kernel) {
  $sum = 0.0;
  $a = 0;
  while ($a < $size_kernel) {
  $b = 0;
  while ($b < $size_kernel) {
  $sum = $sum + $data[$i + $a][$j + $b] * $kernel[$a][$b];
  $b = $b + 1;
};
  $a = $a + 1;
};
  $row = _append($row, sigmoid($sum - $bias));
  $j = $j + $step;
};
  $out = _append($out, $row);
  $i = $i + $step;
};
  return $out;
};
  function average_pool($map, $size) {
  global $seed;
  $out = [];
  $i = 0;
  while ($i < count($map)) {
  $row = [];
  $j = 0;
  while ($j < count($map[$i])) {
  $sum = 0.0;
  $a = 0;
  while ($a < $size) {
  $b = 0;
  while ($b < $size) {
  $sum = $sum + $map[$i + $a][$j + $b];
  $b = $b + 1;
};
  $a = $a + 1;
};
  $row = _append($row, $sum / (floatval(($size * $size))));
  $j = $j + $size;
};
  $out = _append($out, $row);
  $i = $i + $size;
};
  return $out;
};
  function flatten($maps) {
  global $seed;
  $out = [];
  $i = 0;
  while ($i < count($maps)) {
  $j = 0;
  while ($j < count($maps[$i])) {
  $k = 0;
  while ($k < count($maps[$i][$j])) {
  $out = _append($out, $maps[$i][$j][$k]);
  $k = $k + 1;
};
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $out;
};
  function vec_mul_mat($v, $m) {
  global $seed;
  $cols = count($m[0]);
  $res = [];
  $j = 0;
  while ($j < $cols) {
  $sum = 0.0;
  $i = 0;
  while ($i < count($v)) {
  $sum = $sum + $v[$i] * $m[$i][$j];
  $i = $i + 1;
};
  $res = _append($res, $sum);
  $j = $j + 1;
};
  return $res;
};
  function matT_vec_mul($m, $v) {
  global $seed;
  $res = [];
  $i = 0;
  while ($i < count($m)) {
  $sum = 0.0;
  $j = 0;
  while ($j < count($m[$i])) {
  $sum = $sum + $m[$i][$j] * $v[$j];
  $j = $j + 1;
};
  $res = _append($res, $sum);
  $i = $i + 1;
};
  return $res;
};
  function vec_add($a, $b) {
  global $seed;
  $res = [];
  $i = 0;
  while ($i < count($a)) {
  $res = _append($res, $a[$i] + $b[$i]);
  $i = $i + 1;
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
  function vec_map_sig($v) {
  global $seed;
  $res = [];
  $i = 0;
  while ($i < count($v)) {
  $res = _append($res, sigmoid($v[$i]));
  $i = $i + 1;
};
  return $res;
};
  function new_cnn() {
  global $seed;
  $k1 = [[1.0, 0.0], [0.0, 1.0]];
  $k2 = [[0.0, 1.0], [1.0, 0.0]];
  $conv_kernels = [$k1, $k2];
  $conv_bias = [0.0, 0.0];
  $conv_step = 2;
  $pool_size = 2;
  $input_size = 2;
  $hidden_size = 2;
  $output_size = 2;
  $w_hidden = [];
  $i = 0;
  while ($i < $input_size) {
  $row = [];
  $j = 0;
  while ($j < $hidden_size) {
  $row = _append($row, random() - 0.5);
  $j = $j + 1;
};
  $w_hidden = _append($w_hidden, $row);
  $i = $i + 1;
};
  $w_out = [];
  $i = 0;
  while ($i < $hidden_size) {
  $row = [];
  $j = 0;
  while ($j < $output_size) {
  $row = _append($row, random() - 0.5);
  $j = $j + 1;
};
  $w_out = _append($w_out, $row);
  $i = $i + 1;
};
  $b_hidden = [0.0, 0.0];
  $b_out = [0.0, 0.0];
  return ['conv_kernels' => $conv_kernels, 'conv_bias' => $conv_bias, 'conv_step' => $conv_step, 'pool_size' => $pool_size, 'w_hidden' => $w_hidden, 'w_out' => $w_out, 'b_hidden' => $b_hidden, 'b_out' => $b_out, 'rate_weight' => 0.2, 'rate_bias' => 0.2];
};
  function forward($cnn, $data) {
  global $seed;
  $maps = [];
  $i = 0;
  while ($i < _len($cnn['conv_kernels'])) {
  $conv_map = convolve($data, $cnn['conv_kernels'][$i], $cnn['conv_step'], $cnn['conv_bias'][$i]);
  $pooled = average_pool($conv_map, $cnn['pool_size']);
  $maps = _append($maps, $pooled);
  $i = $i + 1;
};
  $flat = flatten($maps);
  $hidden_net = vec_add(vec_mul_mat($flat, $cnn['w_hidden']), $cnn['b_hidden']);
  $hidden_out = vec_map_sig($hidden_net);
  $out_net = vec_add(vec_mul_mat($hidden_out, $cnn['w_out']), $cnn['b_out']);
  $out = vec_map_sig($out_net);
  return $out;
};
  function train($cnn, $samples, $epochs) {
  global $seed;
  $w_out = $cnn['w_out'];
  $b_out = $cnn['b_out'];
  $w_hidden = $cnn['w_hidden'];
  $b_hidden = $cnn['b_hidden'];
  $e = 0;
  while ($e < $epochs) {
  $s = 0;
  while ($s < count($samples)) {
  $data = $samples[$s]['image'];
  $target = $samples[$s]['target'];
  $maps = [];
  $i = 0;
  while ($i < _len($cnn['conv_kernels'])) {
  $conv_map = convolve($data, $cnn['conv_kernels'][$i], $cnn['conv_step'], $cnn['conv_bias'][$i]);
  $pooled = average_pool($conv_map, $cnn['pool_size']);
  $maps = _append($maps, $pooled);
  $i = $i + 1;
};
  $flat = flatten($maps);
  $hidden_net = vec_add(vec_mul_mat($flat, $w_hidden), $b_hidden);
  $hidden_out = vec_map_sig($hidden_net);
  $out_net = vec_add(vec_mul_mat($hidden_out, $w_out), $b_out);
  $out = vec_map_sig($out_net);
  $error_out = vec_sub($target, $out);
  $pd_out = vec_mul($error_out, vec_mul($out, vec_sub([1.0, 1.0], $out)));
  $error_hidden = matT_vec_mul($w_out, $pd_out);
  $pd_hidden = vec_mul($error_hidden, vec_mul($hidden_out, vec_sub([1.0, 1.0], $hidden_out)));
  $j = 0;
  while ($j < count($w_out)) {
  $k = 0;
  while ($k < count($w_out[$j])) {
  $w_out[$j][$k] = $w_out[$j][$k] + $cnn['rate_weight'] * $hidden_out[$j] * $pd_out[$k];
  $k = $k + 1;
};
  $j = $j + 1;
};
  $j = 0;
  while ($j < count($b_out)) {
  $b_out[$j] = $b_out[$j] - $cnn['rate_bias'] * $pd_out[$j];
  $j = $j + 1;
};
  $i_h = 0;
  while ($i_h < count($w_hidden)) {
  $j_h = 0;
  while ($j_h < count($w_hidden[$i_h])) {
  $w_hidden[$i_h][$j_h] = $w_hidden[$i_h][$j_h] + $cnn['rate_weight'] * $flat[$i_h] * $pd_hidden[$j_h];
  $j_h = $j_h + 1;
};
  $i_h = $i_h + 1;
};
  $j = 0;
  while ($j < count($b_hidden)) {
  $b_hidden[$j] = $b_hidden[$j] - $cnn['rate_bias'] * $pd_hidden[$j];
  $j = $j + 1;
};
  $s = $s + 1;
};
  $e = $e + 1;
};
  return ['conv_kernels' => $cnn['conv_kernels'], 'conv_bias' => $cnn['conv_bias'], 'conv_step' => $cnn['conv_step'], 'pool_size' => $cnn['pool_size'], 'w_hidden' => $w_hidden, 'w_out' => $w_out, 'b_hidden' => $b_hidden, 'b_out' => $b_out, 'rate_weight' => $cnn['rate_weight'], 'rate_bias' => $cnn['rate_bias']];
};
  function main() {
  global $seed;
  $cnn = new_cnn();
  $image = [[1.0, 0.0, 1.0, 0.0], [0.0, 1.0, 0.0, 1.0], [1.0, 0.0, 1.0, 0.0], [0.0, 1.0, 0.0, 1.0]];
  $sample = ['image' => $image, 'target' => [1.0, 0.0]];
  echo rtrim('Before training:') . " " . str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(forward($cnn, $image), 1344)))))), PHP_EOL;
  $trained = train($cnn, [$sample], 50);
  echo rtrim('After training:') . " " . str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(forward($trained, $image), 1344)))))), PHP_EOL;
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
