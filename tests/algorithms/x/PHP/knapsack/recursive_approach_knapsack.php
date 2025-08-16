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
  function knapsack($weights, $values, $number_of_items, $max_weight, $index) {
  if ($index == $number_of_items) {
  return 0;
}
  $ans1 = knapsack($weights, $values, $number_of_items, $max_weight, $index + 1);
  $ans2 = 0;
  if ($weights[$index] <= $max_weight) {
  $ans2 = $values[$index] + knapsack($weights, $values, $number_of_items, $max_weight - $weights[$index], $index + 1);
}
  if ($ans1 > $ans2) {
  return $ans1;
}
  return $ans2;
};
  function main() {
  $w1 = [1, 2, 4, 5];
  $v1 = [5, 4, 8, 6];
  $r1 = knapsack($w1, $v1, 4, 5, 0);
  echo rtrim(_str($r1)), PHP_EOL;
  $w2 = [3, 4, 5];
  $v2 = [10, 9, 8];
  $r2 = knapsack($w2, $v2, 3, 25, 0);
  echo rtrim(_str($r2)), PHP_EOL;
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
