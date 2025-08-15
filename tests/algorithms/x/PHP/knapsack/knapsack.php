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
  function knapsack($capacity, $weights, $values, $counter) {
  if ($counter == 0 || $capacity == 0) {
  return 0;
}
  if ($weights[$counter - 1] > $capacity) {
  return knapsack($capacity, $weights, $values, $counter - 1);
} else {
  $left_capacity = $capacity - $weights[$counter - 1];
  $new_value_included = $values[$counter - 1] + knapsack($left_capacity, $weights, $values, $counter - 1);
  $without_new_value = knapsack($capacity, $weights, $values, $counter - 1);
  if ($new_value_included > $without_new_value) {
  return $new_value_included;
} else {
  return $without_new_value;
};
}
};
  function main() {
  $weights = [10, 20, 30];
  $values = [60, 100, 120];
  $cap = 50;
  $count = count($values);
  $result = knapsack($cap, $weights, $values, $count);
  echo rtrim(_str($result)), PHP_EOL;
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
