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
  function sort_by_ratio($index, $ratio) {
  global $v, $w;
  $i = 1;
  while ($i < count($index)) {
  $key = $index[$i];
  $key_ratio = $ratio[$key];
  $j = $i - 1;
  while ($j >= 0 && $ratio[$index[$j]] < $key_ratio) {
  $index[$j + 1] = $index[$j];
  $j = $j - 1;
};
  $index[$j + 1] = $key;
  $i = $i + 1;
};
  return $index;
};
  function fractional_knapsack($value, $weight, $capacity) {
  global $v, $w;
  $n = count($value);
  $index = [];
  $i = 0;
  while ($i < $n) {
  $index = _append($index, $i);
  $i = $i + 1;
};
  $ratio = [];
  $i = 0;
  while ($i < $n) {
  $ratio = _append($ratio, $value[$i] / $weight[$i]);
  $i = $i + 1;
};
  $index = sort_by_ratio($index, $ratio);
  $fractions = [];
  $i = 0;
  while ($i < $n) {
  $fractions = _append($fractions, 0.0);
  $i = $i + 1;
};
  $max_value = 0.0;
  $idx = 0;
  while ($idx < count($index)) {
  $item = $index[$idx];
  if ($weight[$item] <= $capacity) {
  $fractions[$item] = 1.0;
  $max_value = $max_value + $value[$item];
  $capacity = $capacity - $weight[$item];
} else {
  $fractions[$item] = $capacity / $weight[$item];
  $max_value = $max_value + $value[$item] * $capacity / $weight[$item];
  break;
}
  $idx = $idx + 1;
};
  return ['max_value' => $max_value, 'fractions' => $fractions];
};
  $v = [1.0, 3.0, 5.0, 7.0, 9.0];
  $w = [0.9, 0.7, 0.5, 0.3, 0.1];
  echo rtrim(json_encode(fractional_knapsack($v, $w, 5.0), 1344)), PHP_EOL;
  echo rtrim(json_encode(fractional_knapsack([1.0, 3.0, 5.0, 7.0], [0.9, 0.7, 0.5, 0.3], 30.0), 1344)), PHP_EOL;
  echo rtrim(json_encode(fractional_knapsack([], [], 30.0), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
