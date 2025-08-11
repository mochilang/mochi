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
function _iadd($a, $b) {
    if (function_exists('bcadd')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcadd($sa, $sb, 0);
    }
    return $a + $b;
}
function _isub($a, $b) {
    if (function_exists('bcsub')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcsub($sa, $sb, 0);
    }
    return $a - $b;
}
function _imul($a, $b) {
    if (function_exists('bcmul')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcmul($sa, $sb, 0);
    }
    return $a * $b;
}
function _idiv($a, $b) {
    return _intdiv($a, $b);
}
function _imod($a, $b) {
    if (function_exists('bcmod')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcmod($sa, $sb));
    }
    return $a % $b;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function exp_approx($x) {
  global $features, $target, $models, $predictions, $acc;
  $term = 1.0;
  $sum = 1.0;
  $i = 1;
  while ($i < 10) {
  $term = $term * $x / (floatval($i));
  $sum = $sum + $term;
  $i = _iadd($i, 1);
};
  return $sum;
};
  function signf($x) {
  global $features, $target, $models, $predictions, $acc;
  if ($x >= 0.0) {
  return 1.0;
}
  return -1.0;
};
  function gradient($target, $preds) {
  global $features, $models, $predictions, $acc;
  $n = count($target);
  $residuals = [];
  $i = 0;
  while ($i < $n) {
  $t = $target[$i];
  $y = $preds[$i];
  $exp_val = exp_approx($t * $y);
  $res = -$t / (1.0 + $exp_val);
  $residuals = _append($residuals, $res);
  $i = _iadd($i, 1);
};
  return $residuals;
};
  function predict_raw($models, $features, $learning_rate) {
  global $target, $predictions, $acc;
  $n = count($features);
  $preds = [];
  $i = 0;
  while ($i < $n) {
  $preds = _append($preds, 0.0);
  $i = _iadd($i, 1);
};
  $m = 0;
  while ($m < count($models)) {
  $stump = $models[$m];
  $i = 0;
  while ($i < $n) {
  $value = $features[$i][$stump['feature']];
  if ($value <= $stump['threshold']) {
  $preds[$i] = $preds[$i] + $learning_rate * $stump['left'];
} else {
  $preds[$i] = $preds[$i] + $learning_rate * $stump['right'];
}
  $i = _iadd($i, 1);
};
  $m = _iadd($m, 1);
};
  return $preds;
};
  function predict($models, $features, $learning_rate) {
  global $target, $predictions, $acc;
  $raw = predict_raw($models, $features, $learning_rate);
  $result = [];
  $i = 0;
  while ($i < count($raw)) {
  $result = _append($result, signf($raw[$i]));
  $i = _iadd($i, 1);
};
  return $result;
};
  function train_stump($features, $residuals) {
  global $target, $models, $predictions, $acc;
  $n_samples = count($features);
  $n_features = count($features[0]);
  $best_feature = 0;
  $best_threshold = 0.0;
  $best_error = 1000000000.0;
  $best_left = 0.0;
  $best_right = 0.0;
  $j = 0;
  while ($j < $n_features) {
  $t_index = 0;
  while ($t_index < $n_samples) {
  $t = $features[$t_index][$j];
  $sum_left = 0.0;
  $count_left = 0;
  $sum_right = 0.0;
  $count_right = 0;
  $i = 0;
  while ($i < $n_samples) {
  if ($features[$i][$j] <= $t) {
  $sum_left = $sum_left + $residuals[$i];
  $count_left = _iadd($count_left, 1);
} else {
  $sum_right = $sum_right + $residuals[$i];
  $count_right = _iadd($count_right, 1);
}
  $i = _iadd($i, 1);
};
  $left_val = 0.0;
  if ($count_left != 0) {
  $left_val = $sum_left / (floatval($count_left));
}
  $right_val = 0.0;
  if ($count_right != 0) {
  $right_val = $sum_right / (floatval($count_right));
}
  $error = 0.0;
  $i = 0;
  while ($i < $n_samples) {
  $pred = ($features[$i][$j] <= $t ? $left_val : $right_val);
  $diff = $residuals[$i] - $pred;
  $error = $error + $diff * $diff;
  $i = _iadd($i, 1);
};
  if ($error < $best_error) {
  $best_error = $error;
  $best_feature = $j;
  $best_threshold = $t;
  $best_left = $left_val;
  $best_right = $right_val;
}
  $t_index = _iadd($t_index, 1);
};
  $j = _iadd($j, 1);
};
  return ['feature' => $best_feature, 'threshold' => $best_threshold, 'left' => $best_left, 'right' => $best_right];
};
  function fit($n_estimators, $learning_rate, $features, $target) {
  global $predictions, $acc;
  $models = [];
  $m = 0;
  while ($m < $n_estimators) {
  $preds = predict_raw($models, $features, $learning_rate);
  $grad = gradient($target, $preds);
  $residuals = [];
  $i = 0;
  while ($i < count($grad)) {
  $residuals = _append($residuals, -$grad[$i]);
  $i = _iadd($i, 1);
};
  $stump = train_stump($features, $residuals);
  $models = _append($models, $stump);
  $m = _iadd($m, 1);
};
  return $models;
};
  function accuracy($preds, $target) {
  global $features, $models, $predictions, $acc;
  $n = count($target);
  $correct = 0;
  $i = 0;
  while ($i < $n) {
  if ($preds[$i] == $target[$i]) {
  $correct = _iadd($correct, 1);
}
  $i = _iadd($i, 1);
};
  return (floatval($correct)) / (floatval($n));
};
  $features = [[1.0], [2.0], [3.0], [4.0]];
  $target = [-1.0, -1.0, 1.0, 1.0];
  $models = fit(5, 0.5, $features, $target);
  $predictions = predict($models, $features, 0.5);
  $acc = accuracy($predictions, $target);
  echo rtrim('Accuracy: ' . _str($acc)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
