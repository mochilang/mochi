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
  function sqrtApprox($x) {
  global $classes, $knn, $point, $train_X, $train_y;
  if ($x <= 0.0) {
  return 0.0;
}
  $guess = $x;
  $i = 0;
  while ($i < 20) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
};
  function make_knn($train_data, $train_target, $class_labels) {
  global $classes, $knn, $point, $train_X, $train_y;
  $items = [];
  $i = 0;
  while ($i < count($train_data)) {
  $pl = ['point' => $train_data[$i], 'label' => $train_target[$i]];
  $items = _append($items, $pl);
  $i = $i + 1;
};
  return ['data' => $items, 'labels' => $class_labels];
};
  function euclidean_distance($a, $b) {
  global $classes, $knn, $point, $train_X, $train_y;
  $sum = 0.0;
  $i = 0;
  while ($i < count($a)) {
  $diff = $a[$i] - $b[$i];
  $sum = $sum + $diff * $diff;
  $i = $i + 1;
};
  return sqrtApprox($sum);
};
  function classify($knn, $pred_point, $k) {
  global $classes, $point, $train_X, $train_y;
  $distances = [];
  $i = 0;
  while ($i < _len($knn['data'])) {
  $d = euclidean_distance($knn['data'][$i]['point'], $pred_point);
  $distances = _append($distances, ['dist' => $d, 'label' => $knn['data'][$i]['label']]);
  $i = $i + 1;
};
  $votes = [];
  $count = 0;
  while ($count < $k) {
  $min_index = 0;
  $j = 1;
  while ($j < count($distances)) {
  if ($distances[$j]['dist'] < $distances[$min_index]['dist']) {
  $min_index = $j;
}
  $j = $j + 1;
};
  $votes = _append($votes, $distances[$min_index]['label']);
  $distances[$min_index]['dist'] = 1000000000000000000.0;
  $count = $count + 1;
};
  $tally = [];
  $t = 0;
  while ($t < _len($knn['labels'])) {
  $tally = _append($tally, 0);
  $t = $t + 1;
};
  $v = 0;
  while ($v < count($votes)) {
  $lbl = $votes[$v];
  $tally[$lbl] = $tally[$lbl] + 1;
  $v = $v + 1;
};
  $max_idx = 0;
  $m = 1;
  while ($m < count($tally)) {
  if ($tally[$m] > $tally[$max_idx]) {
  $max_idx = $m;
}
  $m = $m + 1;
};
  return $knn['labels'][$max_idx];
};
  $train_X = [[0.0, 0.0], [1.0, 0.0], [0.0, 1.0], [0.5, 0.5], [3.0, 3.0], [2.0, 3.0], [3.0, 2.0]];
  $train_y = [0, 0, 0, 0, 1, 1, 1];
  $classes = ['A', 'B'];
  $knn = make_knn($train_X, $train_y, $classes);
  $point = [1.2, 1.2];
  echo rtrim(classify($knn, $point, 5)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
