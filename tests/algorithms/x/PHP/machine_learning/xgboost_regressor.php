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
  function data_handling($dataset) {
  return $dataset;
};
  function xgboost($features, $target, $test_features) {
  $learning_rate = 0.5;
  $n_estimators = 3;
  $trees = [];
  $predictions = [];
  $i = 0;
  while ($i < count($target)) {
  $predictions = _append($predictions, 0.0);
  $i = $i + 1;
};
  $est = 0;
  while ($est < $n_estimators) {
  $residuals = [];
  $j = 0;
  while ($j < count($target)) {
  $residuals = _append($residuals, $target[$j] - $predictions[$j]);
  $j = $j + 1;
};
  $sum_feat = 0.0;
  $j = 0;
  while ($j < count($features)) {
  $sum_feat = $sum_feat + $features[$j][0];
  $j = $j + 1;
};
  $threshold = $sum_feat / (floatval(count($features)));
  $left_sum = 0.0;
  $left_count = 0;
  $right_sum = 0.0;
  $right_count = 0;
  $j = 0;
  while ($j < count($features)) {
  if ($features[$j][0] <= $threshold) {
  $left_sum = $left_sum + $residuals[$j];
  $left_count = $left_count + 1;
} else {
  $right_sum = $right_sum + $residuals[$j];
  $right_count = $right_count + 1;
}
  $j = $j + 1;
};
  $left_value = 0.0;
  if ($left_count > 0) {
  $left_value = $left_sum / (floatval($left_count));
}
  $right_value = 0.0;
  if ($right_count > 0) {
  $right_value = $right_sum / (floatval($right_count));
}
  $j = 0;
  while ($j < count($features)) {
  if ($features[$j][0] <= $threshold) {
  $predictions[$j] = $predictions[$j] + $learning_rate * $left_value;
} else {
  $predictions[$j] = $predictions[$j] + $learning_rate * $right_value;
}
  $j = $j + 1;
};
  $trees = _append($trees, [$threshold => $threshold, $left_value => $left_value, $right_value => $right_value]);
  $est = $est + 1;
};
  $preds = [];
  $t = 0;
  while ($t < count($test_features)) {
  $pred = 0.0;
  $k = 0;
  while ($k < count($trees)) {
  if ($test_features[$t][0] <= $trees[$k]['threshold']) {
  $pred = $pred + $learning_rate * $trees[$k]['left_value'];
} else {
  $pred = $pred + $learning_rate * $trees[$k]['right_value'];
}
  $k = $k + 1;
};
  $preds = _append($preds, $pred);
  $t = $t + 1;
};
  return $preds;
};
  function mean_absolute_error($y_true, $y_pred) {
  $sum = 0.0;
  $i = 0;
  while ($i < count($y_true)) {
  $diff = $y_true[$i] - $y_pred[$i];
  if ($diff < 0.0) {
  $diff = -$diff;
}
  $sum = $sum + $diff;
  $i = $i + 1;
};
  return $sum / (floatval(count($y_true)));
};
  function mean_squared_error($y_true, $y_pred) {
  $sum = 0.0;
  $i = 0;
  while ($i < count($y_true)) {
  $diff = $y_true[$i] - $y_pred[$i];
  $sum = $sum + $diff * $diff;
  $i = $i + 1;
};
  return $sum / (floatval(count($y_true)));
};
  function main() {
  $california = ['data' => [[1.0], [2.0], [3.0], [4.0]], 'target' => [2.0, 3.0, 4.0, 5.0]];
  $ds = data_handling($california);
  $x_train = $ds['data'];
  $y_train = $ds['target'];
  $x_test = [[1.5], [3.5]];
  $y_test = [2.5, 4.5];
  $predictions = xgboost($x_train, $y_train, $x_test);
  echo rtrim('Predictions:'), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($predictions, 1344)))))), PHP_EOL;
  echo rtrim('Mean Absolute Error:'), PHP_EOL;
  echo rtrim(json_encode(mean_absolute_error($y_test, $predictions), 1344)), PHP_EOL;
  echo rtrim('Mean Square Error:'), PHP_EOL;
  echo rtrim(json_encode(mean_squared_error($y_test, $predictions), 1344)), PHP_EOL;
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
