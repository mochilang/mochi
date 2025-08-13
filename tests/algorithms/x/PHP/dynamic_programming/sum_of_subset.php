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
  function create_bool_matrix($rows, $cols) {
  $matrix = [];
  $i = 0;
  while ($i <= $rows) {
  $row = [];
  $j = 0;
  while ($j <= $cols) {
  $row = _append($row, false);
  $j = $j + 1;
};
  $matrix = _append($matrix, $row);
  $i = $i + 1;
};
  return $matrix;
};
  function is_sum_subset($arr, $required_sum) {
  $arr_len = count($arr);
  $subset = create_bool_matrix($arr_len, $required_sum);
  $i = 0;
  while ($i <= $arr_len) {
  $subset[$i][0] = true;
  $i = $i + 1;
};
  $j = 1;
  while ($j <= $required_sum) {
  $subset[0][$j] = false;
  $j = $j + 1;
};
  $i = 1;
  while ($i <= $arr_len) {
  $j = 1;
  while ($j <= $required_sum) {
  if ($arr[$i - 1] > $j) {
  $subset[$i][$j] = $subset[$i - 1][$j];
}
  if ($arr[$i - 1] <= $j) {
  $subset[$i][$j] = $subset[$i - 1][$j] || $subset[$i - 1][$j - $arr[$i - 1]];
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $subset[$arr_len][$required_sum];
};
  echo rtrim(json_encode(is_sum_subset([2, 4, 6, 8], 5), 1344)), PHP_EOL;
  echo rtrim(json_encode(is_sum_subset([2, 4, 6, 8], 14), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
