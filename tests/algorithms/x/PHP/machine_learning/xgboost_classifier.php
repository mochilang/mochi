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
$__start_mem = memory_get_usage();
$__start = _now();
  function mean($xs) {
  $sum = 0.0;
  $i = 0;
  while ($i < count($xs)) {
  $sum = $sum + $xs[$i];
  $i = $i + 1;
};
  return $sum / (count($xs) * 1.0);
};
  function stump_predict($s, $x) {
  if ($x[$s['feature']] < $s['threshold']) {
  return $s['left'];
}
  return $s['right'];
};
  function train_stump($features, $residuals) {
  $best_feature = 0;
  $best_threshold = 0.0;
  $best_error = 1000000000.0;
  $best_left = 0.0;
  $best_right = 0.0;
  $num_features = count($features[0]);
  $f = 0;
  while ($f < $num_features) {
  $i = 0;
  while ($i < count($features)) {
  $threshold = $features[$i][$f];
  $left = [];
  $right = [];
  $j = 0;
  while ($j < count($features)) {
  if ($features[$j][$f] < $threshold) {
  $left = array_merge($left, [$residuals[$j]]);
} else {
  $right = array_merge($right, [$residuals[$j]]);
}
  $j = $j + 1;
};
  if (count($left) != 0 && count($right) != 0) {
  $left_mean = mean($left);
  $right_mean = mean($right);
  $err = 0.0;
  $j = 0;
  while ($j < count($features)) {
  $pred = ($features[$j][$f] < $threshold ? $left_mean : $right_mean);
  $diff = $residuals[$j] - $pred;
  $err = $err + $diff * $diff;
  $j = $j + 1;
};
  if ($err < $best_error) {
  $best_error = $err;
  $best_feature = $f;
  $best_threshold = $threshold;
  $best_left = $left_mean;
  $best_right = $right_mean;
};
}
  $i = $i + 1;
};
  $f = $f + 1;
};
  return ['feature' => $best_feature, 'threshold' => $best_threshold, 'left' => $best_left, 'right' => $best_right];
};
  function boost($features, $targets, $rounds) {
  $model = [];
  $preds = [];
  $i = 0;
  while ($i < count($targets)) {
  $preds = array_merge($preds, [0.0]);
  $i = $i + 1;
};
  $r = 0;
  while ($r < $rounds) {
  $residuals = [];
  $j = 0;
  while ($j < count($targets)) {
  $residuals = array_merge($residuals, [$targets[$j] - $preds[$j]]);
  $j = $j + 1;
};
  $stump = train_stump($features, $residuals);
  $model = array_merge($model, [$stump]);
  $j = 0;
  while ($j < count($preds)) {
  $preds[$j] = $preds[$j] + stump_predict($stump, $features[$j]);
  $j = $j + 1;
};
  $r = $r + 1;
};
  return $model;
};
  function predict($model, $x) {
  $score = 0.0;
  $i = 0;
  while ($i < count($model)) {
  $s = $model[$i];
  if ($x[$s['feature']] < $s['threshold']) {
  $score = $score + $s['left'];
} else {
  $score = $score + $s['right'];
}
  $i = $i + 1;
};
  return $score;
};
  function main() {
  $features = [[5.1, 3.5], [4.9, 3.0], [6.2, 3.4], [5.9, 3.0]];
  $targets = [0, 0, 1, 1];
  $model = boost($features, $targets, 3);
  $out = '';
  $i = 0;
  while ($i < count($features)) {
  $s = predict($model, $features[$i]);
  $label = ($s >= 0.5 ? 1 : 0);
  if ($i == 0) {
  $out = _str($label);
} else {
  $out = $out . ' ' . _str($label);
}
  $i = $i + 1;
};
  echo rtrim($out), PHP_EOL;
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
