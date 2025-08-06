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
$__start_mem = memory_get_usage();
$__start = _now();
  function knapsack($capacity, $weights, $values, $counter) {
  if ($counter == 0 || $capacity == 0) {
  return 0;
}
  if ($weights[$counter - 1] > $capacity) {
  return knapsack($capacity, $weights, $values, $counter - 1);
}
  $left_capacity = $capacity - $weights[$counter - 1];
  $include_val = $values[$counter - 1] + knapsack($left_capacity, $weights, $values, $counter - 1);
  $exclude_val = knapsack($capacity, $weights, $values, $counter - 1);
  if ($include_val > $exclude_val) {
  return $include_val;
}
  return $exclude_val;
};
  function test_base_case() {
  $cap = 0;
  $val = [0];
  $w = [0];
  $c = count($val);
  if (knapsack($cap, $w, $val, $c) != 0) {
  return false;
}
  $val2 = [60];
  $w2 = [10];
  $c2 = count($val2);
  return knapsack($cap, $w2, $val2, $c2) == 0;
};
  function test_easy_case() {
  $cap = 3;
  $val = [1, 2, 3];
  $w = [3, 2, 1];
  $c = count($val);
  return knapsack($cap, $w, $val, $c) == 5;
};
  function test_knapsack() {
  $cap = 50;
  $val = [60, 100, 120];
  $w = [10, 20, 30];
  $c = count($val);
  return knapsack($cap, $w, $val, $c) == 220;
};
  echo rtrim(json_encode(test_base_case(), 1344)), PHP_EOL;
  echo rtrim(json_encode(test_easy_case(), 1344)), PHP_EOL;
  echo rtrim(json_encode(test_knapsack(), 1344)), PHP_EOL;
  echo rtrim((true ? 'true' : 'false')), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
