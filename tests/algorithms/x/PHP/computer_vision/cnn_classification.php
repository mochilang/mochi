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
  function conv2d($image, $kernel) {
  global $conv, $activated, $pooled, $flat, $weights, $bias, $probability;
  $rows = count($image);
  $cols = count($image[0]);
  $k = count($kernel);
  $output = [];
  $i = 0;
  while ($i <= $rows - $k) {
  $row = [];
  $j = 0;
  while ($j <= $cols - $k) {
  $sum = 0.0;
  $ki = 0;
  while ($ki < $k) {
  $kj = 0;
  while ($kj < $k) {
  $sum = $sum + $image[$i + $ki][$j + $kj] * $kernel[$ki][$kj];
  $kj = $kj + 1;
};
  $ki = $ki + 1;
};
  $row = _append($row, $sum);
  $j = $j + 1;
};
  $output = _append($output, $row);
  $i = $i + 1;
};
  return $output;
};
  function relu_matrix($m) {
  global $image, $kernel, $conv, $activated, $pooled, $flat, $weights, $bias, $output, $probability;
  $out = [];
  foreach ($m as $row) {
  $new_row = [];
  foreach ($row as $v) {
  if ($v > 0.0) {
  $new_row = _append($new_row, $v);
} else {
  $new_row = _append($new_row, 0.0);
}
};
  $out = _append($out, $new_row);
};
  return $out;
};
  function max_pool2x2($m) {
  global $image, $kernel, $conv, $activated, $pooled, $flat, $weights, $bias, $output, $probability;
  $rows = count($m);
  $cols = count($m[0]);
  $out = [];
  $i = 0;
  while ($i < $rows) {
  $new_row = [];
  $j = 0;
  while ($j < $cols) {
  $max_val = $m[$i][$j];
  if ($m[$i][$j + 1] > $max_val) {
  $max_val = $m[$i][$j + 1];
}
  if ($m[$i + 1][$j] > $max_val) {
  $max_val = $m[$i + 1][$j];
}
  if ($m[$i + 1][$j + 1] > $max_val) {
  $max_val = $m[$i + 1][$j + 1];
}
  $new_row = _append($new_row, $max_val);
  $j = $j + 2;
};
  $out = _append($out, $new_row);
  $i = $i + 2;
};
  return $out;
};
  function flatten($m) {
  global $image, $kernel, $conv, $activated, $pooled, $flat, $weights, $bias, $output, $probability;
  $res = [];
  foreach ($m as $row) {
  foreach ($row as $v) {
  $res = _append($res, $v);
};
};
  return $res;
};
  function dense($inputs, $weights, $bias) {
  global $image, $kernel, $conv, $activated, $pooled, $flat, $output, $probability;
  $s = $bias;
  $i = 0;
  while ($i < count($inputs)) {
  $s = $s + $inputs[$i] * $weights[$i];
  $i = $i + 1;
};
  return $s;
};
  function exp_approx($x) {
  global $image, $kernel, $conv, $activated, $pooled, $flat, $weights, $bias, $output, $probability;
  $sum = 1.0;
  $term = 1.0;
  $i = 1;
  while ($i <= 10) {
  $term = $term * $x / $i;
  $sum = $sum + $term;
  $i = $i + 1;
};
  return $sum;
};
  function sigmoid($x) {
  global $image, $kernel, $conv, $activated, $pooled, $flat, $weights, $bias, $output, $probability;
  return 1.0 / (1.0 + exp_approx(-$x));
};
  $image = [[0.0, 1.0, 1.0, 0.0, 0.0, 0.0], [0.0, 1.0, 1.0, 0.0, 0.0, 0.0], [0.0, 0.0, 1.0, 1.0, 0.0, 0.0], [0.0, 0.0, 1.0, 1.0, 0.0, 0.0], [0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.0, 0.0, 0.0, 0.0]];
  $kernel = [[1.0, 0.0, -1.0], [1.0, 0.0, -1.0], [1.0, 0.0, -1.0]];
  $conv = conv2d($image, $kernel);
  $activated = relu_matrix($conv);
  $pooled = max_pool2x2($activated);
  $flat = flatten($pooled);
  $weights = [0.5, -0.4, 0.3, 0.1];
  $bias = 0.0;
  $output = dense($flat, $weights, $bias);
  $probability = sigmoid($output);
  if ($probability >= 0.5) {
  echo rtrim('Abnormality detected'), PHP_EOL;
} else {
  echo rtrim('Normal'), PHP_EOL;
}
  echo rtrim('Probability:'), PHP_EOL;
  echo rtrim(json_encode($probability, 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
