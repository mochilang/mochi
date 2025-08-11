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
  function distance_sq($a, $b) {
  global $data, $k, $initial_centroids, $result;
  $sum = 0.0;
  for ($i = 0; $i < count($a); $i++) {
  $diff = $a[$i] - $b[$i];
  $sum = $sum + $diff * $diff;
};
  return $sum;
};
  function assign_clusters($data, $centroids) {
  global $k, $initial_centroids, $result;
  $assignments = [];
  for ($i = 0; $i < count($data); $i++) {
  $best_idx = 0;
  $best = distance_sq($data[$i], $centroids[0]);
  for ($j = 1; $j < count($centroids); $j++) {
  $dist = distance_sq($data[$i], $centroids[$j]);
  if ($dist < $best) {
  $best = $dist;
  $best_idx = $j;
}
};
  $assignments = _append($assignments, $best_idx);
};
  return $assignments;
};
  function revise_centroids($data, $k, $assignment) {
  global $initial_centroids, $result;
  $dim = count($data[0]);
  $sums = [];
  $counts = [];
  for ($i = 0; $i < $k; $i++) {
  $row = [];
  for ($j = 0; $j < $dim; $j++) {
  $row = _append($row, 0.0);
};
  $sums = _append($sums, $row);
  $counts = _append($counts, 0);
};
  for ($i = 0; $i < count($data); $i++) {
  $c = $assignment[$i];
  $counts[$c] = _iadd($counts[$c], 1);
  for ($j = 0; $j < $dim; $j++) {
  $sums[$c][$j] = $sums[$c][$j] + $data[$i][$j];
};
};
  $centroids = [];
  for ($i = 0; $i < $k; $i++) {
  $row = [];
  if ($counts[$i] > 0) {
  for ($j = 0; $j < $dim; $j++) {
  $row = _append($row, $sums[$i][$j] / (floatval($counts[$i])));
};
} else {
  for ($j = 0; $j < $dim; $j++) {
  $row = _append($row, 0.0);
};
}
  $centroids = _append($centroids, $row);
};
  return $centroids;
};
  function compute_heterogeneity($data, $centroids, $assignment) {
  global $k, $initial_centroids, $result;
  $total = 0.0;
  for ($i = 0; $i < count($data); $i++) {
  $c = $assignment[$i];
  $total = $total + distance_sq($data[$i], $centroids[$c]);
};
  return $total;
};
  function lists_equal($a, $b) {
  global $data, $k, $initial_centroids, $result;
  if (count($a) != count($b)) {
  return false;
}
  for ($i = 0; $i < count($a); $i++) {
  if ($a[$i] != $b[$i]) {
  return false;
}
};
  return true;
};
  function kmeans($data, $k, $initial_centroids, $max_iter) {
  global $result;
  $centroids = $initial_centroids;
  $assignment = [];
  $prev = [];
  $heterogeneity = [];
  $iter = 0;
  while ($iter < $max_iter) {
  $assignment = assign_clusters($data, $centroids);
  $centroids = revise_centroids($data, $k, $assignment);
  $h = compute_heterogeneity($data, $centroids, $assignment);
  $heterogeneity = _append($heterogeneity, $h);
  if ($iter > 0 && lists_equal($prev, $assignment)) {
  break;
}
  $prev = $assignment;
  $iter = _iadd($iter, 1);
};
  return ['centroids' => $centroids, 'assignments' => $assignment, 'heterogeneity' => $heterogeneity];
};
  $data = [[1.0, 2.0], [1.5, 1.8], [5.0, 8.0], [8.0, 8.0], [1.0, 0.6], [9.0, 11.0]];
  $k = 3;
  $initial_centroids = [$data[0], $data[2], $data[5]];
  $result = kmeans($data, $k, $initial_centroids, 10);
  echo rtrim(_str($result['centroids'])), PHP_EOL;
  echo rtrim(_str($result['assignments'])), PHP_EOL;
  echo rtrim(_str($result['heterogeneity'])), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
