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
  $PI = 3.141592653589793;
  $TWO_PI = 6.283185307179586;
  $seed = 1;
  function mochi_rand() {
  global $PI, $TWO_PI, $seed;
  $seed = ($seed * 1103515245 + 12345) % 2147483648;
  return $seed;
};
  function random() {
  global $PI, $TWO_PI, $seed;
  return (floatval(mochi_rand())) / 2147483648.0;
};
  function _mod($x, $m) {
  global $PI, $TWO_PI, $seed;
  return $x - (floatval(intval($x / $m))) * $m;
};
  function cos($x) {
  global $PI, $TWO_PI, $seed;
  $y = _mod($x + $PI, $TWO_PI) - $PI;
  $y2 = $y * $y;
  $y4 = $y2 * $y2;
  $y6 = $y4 * $y2;
  return 1.0 - $y2 / 2.0 + $y4 / 24.0 - $y6 / 720.0;
};
  function sqrtApprox($x) {
  global $PI, $TWO_PI, $seed;
  if ($x <= 0.0) {
  return 0.0;
}
  $guess = $x;
  $i = 0;
  while ($i < 10) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
};
  function ln($x) {
  global $PI, $TWO_PI, $seed;
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
  function gaussian_distribution($mean, $std_dev, $instance_count) {
  global $PI, $TWO_PI, $seed;
  $res = [];
  $i = 0;
  while ($i < $instance_count) {
  $u1 = random();
  $u2 = random();
  $r = sqrtApprox(-2.0 * ln($u1));
  $theta = $TWO_PI * $u2;
  $z = $r * cos($theta);
  $res = _append($res, $mean + $z * $std_dev);
  $i = $i + 1;
};
  return $res;
};
  function y_generator($class_count, $instance_count) {
  global $PI, $TWO_PI, $seed;
  $res = [];
  $k = 0;
  while ($k < $class_count) {
  $i = 0;
  while ($i < $instance_count[$k]) {
  $res = _append($res, $k);
  $i = $i + 1;
};
  $k = $k + 1;
};
  return $res;
};
  function calculate_mean($instance_count, $items) {
  global $PI, $TWO_PI, $seed;
  $total = 0.0;
  $i = 0;
  while ($i < $instance_count) {
  $total = $total + $items[$i];
  $i = $i + 1;
};
  return $total / (floatval($instance_count));
};
  function calculate_probabilities($instance_count, $total_count) {
  global $PI, $TWO_PI, $seed;
  return (floatval($instance_count)) / (floatval($total_count));
};
  function calculate_variance($items, $means, $total_count) {
  global $PI, $TWO_PI, $seed;
  $squared_diff = [];
  $i = 0;
  while ($i < count($items)) {
  $j = 0;
  while ($j < count($items[$i])) {
  $diff = $items[$i][$j] - $means[$i];
  $squared_diff = _append($squared_diff, $diff * $diff);
  $j = $j + 1;
};
  $i = $i + 1;
};
  $sum_sq = 0.0;
  $k = 0;
  while ($k < count($squared_diff)) {
  $sum_sq = $sum_sq + $squared_diff[$k];
  $k = $k + 1;
};
  $n_classes = count($means);
  return (1.0 / (floatval(($total_count - $n_classes)))) * $sum_sq;
};
  function predict_y_values($x_items, $means, $variance, $probabilities) {
  global $PI, $TWO_PI, $seed;
  $results = [];
  $i = 0;
  while ($i < count($x_items)) {
  $j = 0;
  while ($j < count($x_items[$i])) {
  $temp = [];
  $k = 0;
  while ($k < count($x_items)) {
  $discr = $x_items[$i][$j] * ($means[$k] / $variance) - ($means[$k] * $means[$k]) / (2.0 * $variance) + ln($probabilities[$k]);
  $temp = _append($temp, $discr);
  $k = $k + 1;
};
  $max_idx = 0;
  $max_val = $temp[0];
  $t = 1;
  while ($t < count($temp)) {
  if ($temp[$t] > $max_val) {
  $max_val = $temp[$t];
  $max_idx = $t;
}
  $t = $t + 1;
};
  $results = _append($results, $max_idx);
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $results;
};
  function accuracy($actual_y, $predicted_y) {
  global $PI, $TWO_PI, $seed;
  $correct = 0;
  $i = 0;
  while ($i < count($actual_y)) {
  if ($actual_y[$i] == $predicted_y[$i]) {
  $correct = $correct + 1;
}
  $i = $i + 1;
};
  return (floatval($correct)) / (floatval(count($actual_y))) * 100.0;
};
  function main() {
  global $PI, $TWO_PI, $seed;
  $seed = 1;
  $counts = [20, 20, 20];
  $means = [5.0, 10.0, 15.0];
  $std_dev = 1.0;
  $x = [];
  $i = 0;
  while ($i < count($counts)) {
  $x = _append($x, gaussian_distribution($means[$i], $std_dev, $counts[$i]));
  $i = $i + 1;
};
  $y = y_generator(count($counts), $counts);
  $actual_means = [];
  $i = 0;
  while ($i < count($counts)) {
  $actual_means = _append($actual_means, calculate_mean($counts[$i], $x[$i]));
  $i = $i + 1;
};
  $total_count = 0;
  $i = 0;
  while ($i < count($counts)) {
  $total_count = $total_count + $counts[$i];
  $i = $i + 1;
};
  $probabilities = [];
  $i = 0;
  while ($i < count($counts)) {
  $probabilities = _append($probabilities, calculate_probabilities($counts[$i], $total_count));
  $i = $i + 1;
};
  $variance = calculate_variance($x, $actual_means, $total_count);
  $predicted = predict_y_values($x, $actual_means, $variance, $probabilities);
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($predicted, 1344)))))), PHP_EOL;
  echo rtrim(json_encode(accuracy($y, $predicted), 1344)), PHP_EOL;
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
