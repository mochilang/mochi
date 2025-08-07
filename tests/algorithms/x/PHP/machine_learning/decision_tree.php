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
$__start_mem = memory_get_usage();
$__start = _now();
  $PI = 3.141592653589793;
  $TWO_PI = 6.283185307179586;
  function _mod($x, $m) {
  global $PI, $TWO_PI, $seed;
  return $x - (floatval(intval($x / $m))) * $m;
};
  function sin($x) {
  global $PI, $TWO_PI, $seed;
  $y = _mod($x + $PI, $TWO_PI) - $PI;
  $y2 = $y * $y;
  $y3 = $y2 * $y;
  $y5 = $y3 * $y2;
  $y7 = $y5 * $y2;
  return $y - $y3 / 6.0 + $y5 / 120.0 - $y7 / 5040.0;
};
  $seed = 123456789;
  function mochi_rand() {
  global $PI, $TWO_PI, $seed;
  $seed = (1103515245 * $seed + 12345) % 2147483648;
  return floatval($seed) / 2147483648.0;
};
  function mean($vals) {
  global $PI, $TWO_PI, $seed;
  $sum = 0.0;
  $i = 0;
  while ($i < count($vals)) {
  $sum = $sum + $vals[$i];
  $i = $i + 1;
};
  return $sum / count($vals);
};
  function mean_squared_error($labels, $prediction) {
  global $PI, $TWO_PI, $seed;
  $total = 0.0;
  $i = 0;
  while ($i < count($labels)) {
  $diff = $labels[$i] - $prediction;
  $total = $total + $diff * $diff;
  $i = $i + 1;
};
  return $total / count($labels);
};
  function train_tree($x, $y, $depth, $min_leaf_size) {
  global $PI, $TWO_PI, $seed;
  if (count($x) < 2 * $min_leaf_size) {
  return ['__tag' => 'Leaf', 'prediction' => mean($y)];
}
  if ($depth == 1) {
  return ['__tag' => 'Leaf', 'prediction' => mean($y)];
}
  $best_split = 0;
  $min_error = mean_squared_error($x, mean($y)) * 2.0;
  $i = 0;
  while ($i < count($x)) {
  if (count(array_slice($x, 0, $i - 0)) < $min_leaf_size) {
  $i = $i;
} else {
  if (count(array_slice($x, $i)) < $min_leaf_size) {
  $i = $i;
} else {
  $err_left = mean_squared_error(array_slice($x, 0, $i - 0), mean(array_slice($y, 0, $i - 0)));
  $err_right = mean_squared_error(array_slice($x, $i), mean(array_slice($y, $i)));
  $err = $err_left + $err_right;
  if ($err < $min_error) {
  $best_split = $i;
  $min_error = $err;
};
};
}
  $i = $i + 1;
};
  if ($best_split != 0) {
  $left_x = array_slice($x, 0, $best_split - 0);
  $left_y = array_slice($y, 0, $best_split - 0);
  $right_x = array_slice($x, $best_split);
  $right_y = array_slice($y, $best_split);
  $boundary = $x[$best_split];
  $left_tree = train_tree($left_x, $left_y, $depth - 1, $min_leaf_size);
  $right_tree = train_tree($right_x, $right_y, $depth - 1, $min_leaf_size);
  return ['__tag' => 'Branch', 'decision_boundary' => $boundary, 'left' => $left_tree, 'right' => $right_tree];
}
  return ['__tag' => 'Leaf', 'prediction' => mean($y)];
};
  function predict($tree, $value) {
  global $PI, $TWO_PI, $seed;
  return match($tree) {
    $Leaf($p) => $p,
    $Branch($b, $l, $r) => ($value >= $b ? predict($r, $value) : predict($l, $value)),
};
};
  function main() {
  global $PI, $TWO_PI, $seed;
  $x = [];
  $v = -1.0;
  while ($v < 1.0) {
  $x = _append($x, $v);
  $v = $v + 0.005;
};
  $y = [];
  $i = 0;
  while ($i < count($x)) {
  $y = _append($y, sin($x[$i]));
  $i = $i + 1;
};
  $tree = train_tree($x, $y, 10, 10);
  $test_cases = [];
  $i = 0;
  while ($i < 10) {
  $test_cases = _append($test_cases, mochi_rand() * 2.0 - 1.0);
  $i = $i + 1;
};
  $predictions = [];
  $i = 0;
  while ($i < count($test_cases)) {
  $predictions = _append($predictions, predict($tree, $test_cases[$i]));
  $i = $i + 1;
};
  $sum_err = 0.0;
  $i = 0;
  while ($i < count($test_cases)) {
  $diff = $predictions[$i] - $test_cases[$i];
  $sum_err = $sum_err + $diff * $diff;
  $i = $i + 1;
};
  $avg_error = $sum_err / count($test_cases);
  echo rtrim('Test values: ' . _str($test_cases)), PHP_EOL;
  echo rtrim('Predictions: ' . _str($predictions)), PHP_EOL;
  echo rtrim('Average error: ' . _str($avg_error)), PHP_EOL;
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
