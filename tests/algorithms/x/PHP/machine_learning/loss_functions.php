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
  if ($x < 0.0) {
  return -$x;
}
  return $x;
};
  function maxf($a, $b) {
  if ($a > $b) {
  return $a;
}
  return $b;
};
  function minf($a, $b) {
  if ($a < $b) {
  return $a;
}
  return $b;
};
  function clip($x, $lo, $hi) {
  return maxf($lo, minf($x, $hi));
};
  function to_float($x) {
  return $x * 1.0;
};
  function powf($base, $exp) {
  $result = 1.0;
  $i = 0;
  $n = intval('mochi_exp');
  while ($i < $n) {
  $result = $result * $base;
  $i = $i + 1;
};
  return $result;
};
  function ln($x) {
  if ($x <= 0.0) {
  $panic('ln domain error');
}
  $y = ($x - 1.0) / ($x + 1.0);
  $y2 = $y * $y;
  $term = $y;
  $sum = 0.0;
  $k = 0;
  while ($k < 10) {
  $denom = to_float(2 * $k + 1);
  $sum = $sum + $term / $denom;
  $term = $term * $y2;
  $k = $k + 1;
};
  return 2.0 * $sum;
};
  function mochi_exp($x) {
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
  function mean($v) {
  $total = 0.0;
  $i = 0;
  while ($i < count($v)) {
  $total = $total + $v[$i];
  $i = $i + 1;
};
  return $total / to_float(count($v));
};
  function binary_cross_entropy($y_true, $y_pred, $epsilon) {
  if (count($y_true) != count($y_pred)) {
  $panic('Input arrays must have the same length.');
}
  $losses = [];
  $i = 0;
  while ($i < count($y_true)) {
  $yt = $y_true[$i];
  $yp = clip($y_pred[$i], $epsilon, 1.0 - $epsilon);
  $loss = -($yt * ln($yp) + (1.0 - $yt) * ln(1.0 - $yp));
  $losses = _append($losses, $loss);
  $i = $i + 1;
};
  return mean($losses);
};
  function binary_focal_cross_entropy($y_true, $y_pred, $gamma, $alpha, $epsilon) {
  if (count($y_true) != count($y_pred)) {
  $panic('Input arrays must have the same length.');
}
  $losses = [];
  $i = 0;
  while ($i < count($y_true)) {
  $yt = $y_true[$i];
  $yp = clip($y_pred[$i], $epsilon, 1.0 - $epsilon);
  $term1 = $alpha * powf(1.0 - $yp, $gamma) * $yt * ln($yp);
  $term2 = (1.0 - $alpha) * powf($yp, $gamma) * (1.0 - $yt) * ln(1.0 - $yp);
  $losses = _append($losses, -($term1 + $term2));
  $i = $i + 1;
};
  return mean($losses);
};
  function categorical_cross_entropy($y_true, $y_pred, $epsilon) {
  if (count($y_true) != count($y_pred)) {
  $panic('Input arrays must have the same shape.');
}
  $rows = count($y_true);
  $total = 0.0;
  $i = 0;
  while ($i < $rows) {
  if (count($y_true[$i]) != count($y_pred[$i])) {
  $panic('Input arrays must have the same shape.');
}
  $sum_true = 0.0;
  $sum_pred = 0.0;
  $j = 0;
  while ($j < count($y_true[$i])) {
  $yt = $y_true[$i][$j];
  $yp = $y_pred[$i][$j];
  if (($yt != 0.0 && $yt != 1.0)) {
  $panic('y_true must be one-hot encoded.');
}
  $sum_true = $sum_true + $yt;
  $sum_pred = $sum_pred + $yp;
  $j = $j + 1;
};
  if ($sum_true != 1.0) {
  $panic('y_true must be one-hot encoded.');
}
  if (absf($sum_pred - 1.0) > $epsilon) {
  $panic('Predicted probabilities must sum to approximately 1.');
}
  $j = 0;
  while ($j < count($y_true[$i])) {
  $yp = clip($y_pred[$i][$j], $epsilon, 1.0);
  $total = $total - ($y_true[$i][$j] * ln($yp));
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $total;
};
  function categorical_focal_cross_entropy($y_true, $y_pred, $alpha, $gamma, $epsilon) {
  if (count($y_true) != count($y_pred)) {
  $panic('Shape of y_true and y_pred must be the same.');
}
  $rows = count($y_true);
  $cols = count($y_true[0]);
  $a = $alpha;
  if (count($a) == 0) {
  $tmp = [];
  $j = 0;
  while ($j < $cols) {
  $tmp = _append($tmp, 1.0);
  $j = $j + 1;
};
  $a = $tmp;
}
  if (count($a) != $cols) {
  $panic('Length of alpha must match the number of classes.');
}
  $total = 0.0;
  $i = 0;
  while ($i < $rows) {
  if (count($y_true[$i]) != $cols || count($y_pred[$i]) != $cols) {
  $panic('Shape of y_true and y_pred must be the same.');
}
  $sum_true = 0.0;
  $sum_pred = 0.0;
  $j = 0;
  while ($j < $cols) {
  $yt = $y_true[$i][$j];
  $yp = $y_pred[$i][$j];
  if (($yt != 0.0 && $yt != 1.0)) {
  $panic('y_true must be one-hot encoded.');
}
  $sum_true = $sum_true + $yt;
  $sum_pred = $sum_pred + $yp;
  $j = $j + 1;
};
  if ($sum_true != 1.0) {
  $panic('y_true must be one-hot encoded.');
}
  if (absf($sum_pred - 1.0) > $epsilon) {
  $panic('Predicted probabilities must sum to approximately 1.');
}
  $row_loss = 0.0;
  $j = 0;
  while ($j < $cols) {
  $yp = clip($y_pred[$i][$j], $epsilon, 1.0);
  $row_loss = $row_loss + $a[$j] * powf(1.0 - $yp, $gamma) * $y_true[$i][$j] * ln($yp);
  $j = $j + 1;
};
  $total = $total - $row_loss;
  $i = $i + 1;
};
  return $total / to_float($rows);
};
  function hinge_loss($y_true, $y_pred) {
  if (count($y_true) != count($y_pred)) {
  $panic('Length of predicted and actual array must be same.');
}
  $losses = [];
  $i = 0;
  while ($i < count($y_true)) {
  $yt = $y_true[$i];
  if (($yt != (-1.0) && $yt != 1.0)) {
  $panic('y_true can have values -1 or 1 only.');
}
  $pred = $y_pred[$i];
  $l = maxf(0.0, 1.0 - $yt * $pred);
  $losses = _append($losses, $l);
  $i = $i + 1;
};
  return mean($losses);
};
  function huber_loss($y_true, $y_pred, $delta) {
  if (count($y_true) != count($y_pred)) {
  $panic('Input arrays must have the same length.');
}
  $total = 0.0;
  $i = 0;
  while ($i < count($y_true)) {
  $diff = $y_true[$i] - $y_pred[$i];
  $adiff = absf($diff);
  if ($adiff <= $delta) {
  $total = $total + 0.5 * $diff * $diff;
} else {
  $total = $total + $delta * ($adiff - 0.5 * $delta);
}
  $i = $i + 1;
};
  return $total / to_float(count($y_true));
};
  function mean_squared_error($y_true, $y_pred) {
  if (count($y_true) != count($y_pred)) {
  $panic('Input arrays must have the same length.');
}
  $losses = [];
  $i = 0;
  while ($i < count($y_true)) {
  $diff = $y_true[$i] - $y_pred[$i];
  $losses = _append($losses, $diff * $diff);
  $i = $i + 1;
};
  return mean($losses);
};
  function mean_absolute_error($y_true, $y_pred) {
  if (count($y_true) != count($y_pred)) {
  $panic('Input arrays must have the same length.');
}
  $total = 0.0;
  $i = 0;
  while ($i < count($y_true)) {
  $total = $total + absf($y_true[$i] - $y_pred[$i]);
  $i = $i + 1;
};
  return $total / to_float(count($y_true));
};
  function mean_squared_logarithmic_error($y_true, $y_pred) {
  if (count($y_true) != count($y_pred)) {
  $panic('Input arrays must have the same length.');
}
  $total = 0.0;
  $i = 0;
  while ($i < count($y_true)) {
  $a = ln(1.0 + $y_true[$i]);
  $b = ln(1.0 + $y_pred[$i]);
  $diff = $a - $b;
  $total = $total + $diff * $diff;
  $i = $i + 1;
};
  return $total / to_float(count($y_true));
};
  function mean_absolute_percentage_error($y_true, $y_pred, $epsilon) {
  if (count($y_true) != count($y_pred)) {
  $panic('The length of the two arrays should be the same.');
}
  $total = 0.0;
  $i = 0;
  while ($i < count($y_true)) {
  $yt = $y_true[$i];
  if ($yt == 0.0) {
  $yt = $epsilon;
}
  $total = $total + absf(($yt - $y_pred[$i]) / $yt);
  $i = $i + 1;
};
  return $total / to_float(count($y_true));
};
  function perplexity_loss($y_true, $y_pred, $epsilon) {
  $batch = count($y_true);
  if ($batch != count($y_pred)) {
  $panic('Batch size of y_true and y_pred must be equal.');
}
  $sentence_len = count($y_true[0]);
  if ($sentence_len != count($y_pred[0])) {
  $panic('Sentence length of y_true and y_pred must be equal.');
}
  $vocab_size = count($y_pred[0][0]);
  $b = 0;
  $total_perp = 0.0;
  while ($b < $batch) {
  if (count($y_true[$b]) != $sentence_len || count($y_pred[$b]) != $sentence_len) {
  $panic('Sentence length of y_true and y_pred must be equal.');
}
  $sum_log = 0.0;
  $j = 0;
  while ($j < $sentence_len) {
  $label = $y_true[$b][$j];
  if ($label >= $vocab_size) {
  $panic('Label value must not be greater than vocabulary size.');
}
  $prob = clip($y_pred[$b][$j][$label], $epsilon, 1.0);
  $sum_log = $sum_log + ln($prob);
  $j = $j + 1;
};
  $mean_log = $sum_log / to_float($sentence_len);
  $perp = mochi_exp(-$mean_log);
  $total_perp = $total_perp + $perp;
  $b = $b + 1;
};
  return $total_perp / to_float($batch);
};
  function smooth_l1_loss($y_true, $y_pred, $beta) {
  if (count($y_true) != count($y_pred)) {
  $panic('The length of the two arrays should be the same.');
}
  $total = 0.0;
  $i = 0;
  while ($i < count($y_true)) {
  $diff = absf($y_true[$i] - $y_pred[$i]);
  if ($diff < $beta) {
  $total = $total + 0.5 * $diff * $diff / $beta;
} else {
  $total = $total + $diff - 0.5 * $beta;
}
  $i = $i + 1;
};
  return $total / to_float(count($y_true));
};
  function kullback_leibler_divergence($y_true, $y_pred) {
  if (count($y_true) != count($y_pred)) {
  $panic('Input arrays must have the same length.');
}
  $total = 0.0;
  $i = 0;
  while ($i < count($y_true)) {
  $total = $total + $y_true[$i] * ln($y_true[$i] / $y_pred[$i]);
  $i = $i + 1;
};
  return $total;
};
  function main() {
  $y_true_bc = [0.0, 1.0, 1.0, 0.0, 1.0];
  $y_pred_bc = [0.2, 0.7, 0.9, 0.3, 0.8];
  echo rtrim(json_encode(binary_cross_entropy($y_true_bc, $y_pred_bc, 0.000000000000001), 1344)), PHP_EOL;
  echo rtrim(json_encode(binary_focal_cross_entropy($y_true_bc, $y_pred_bc, 2.0, 0.25, 0.000000000000001), 1344)), PHP_EOL;
  $y_true_cce = [[1.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0.0, 0.0, 1.0]];
  $y_pred_cce = [[0.9, 0.1, 0.0], [0.2, 0.7, 0.1], [0.0, 0.1, 0.9]];
  echo rtrim(json_encode(categorical_cross_entropy($y_true_cce, $y_pred_cce, 0.000000000000001), 1344)), PHP_EOL;
  $alpha = [0.6, 0.2, 0.7];
  echo rtrim(json_encode(categorical_focal_cross_entropy($y_true_cce, $y_pred_cce, $alpha, 2.0, 0.000000000000001), 1344)), PHP_EOL;
  $y_true_hinge = [-1.0, 1.0, 1.0, -1.0, 1.0];
  $y_pred_hinge = [-4.0, -0.3, 0.7, 5.0, 10.0];
  echo rtrim(json_encode(hinge_loss($y_true_hinge, $y_pred_hinge), 1344)), PHP_EOL;
  $y_true_huber = [0.9, 10.0, 2.0, 1.0, 5.2];
  $y_pred_huber = [0.8, 2.1, 2.9, 4.2, 5.2];
  echo rtrim(json_encode(huber_loss($y_true_huber, $y_pred_huber, 1.0), 1344)), PHP_EOL;
  echo rtrim(json_encode(mean_squared_error($y_true_huber, $y_pred_huber), 1344)), PHP_EOL;
  echo rtrim(json_encode(mean_absolute_error($y_true_huber, $y_pred_huber), 1344)), PHP_EOL;
  echo rtrim(json_encode(mean_squared_logarithmic_error($y_true_huber, $y_pred_huber), 1344)), PHP_EOL;
  $y_true_mape = [10.0, 20.0, 30.0, 40.0];
  $y_pred_mape = [12.0, 18.0, 33.0, 45.0];
  echo rtrim(json_encode(mean_absolute_percentage_error($y_true_mape, $y_pred_mape, 0.000000000000001), 1344)), PHP_EOL;
  $y_true_perp = [[1, 4], [2, 3]];
  $y_pred_perp = [[[0.28, 0.19, 0.21, 0.15, 0.17], [0.24, 0.19, 0.09, 0.18, 0.3]], [[0.03, 0.26, 0.21, 0.18, 0.32], [0.28, 0.1, 0.33, 0.15, 0.14]]];
  echo rtrim(json_encode(perplexity_loss($y_true_perp, $y_pred_perp, 0.0000001), 1344)), PHP_EOL;
  $y_true_smooth = [3.0, 5.0, 2.0, 7.0];
  $y_pred_smooth = [2.9, 4.8, 2.1, 7.2];
  echo rtrim(json_encode(smooth_l1_loss($y_true_smooth, $y_pred_smooth, 1.0), 1344)), PHP_EOL;
  $y_true_kl = [0.2, 0.3, 0.5];
  $y_pred_kl = [0.3, 0.3, 0.4];
  echo rtrim(json_encode(kullback_leibler_divergence($y_true_kl, $y_pred_kl), 1344)), PHP_EOL;
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
