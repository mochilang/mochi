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
  function abs_val($num) {
  if ($num < 0.0) {
  return -$num;
}
  return $num;
};
  function abs_min($x) {
  if (count($x) == 0) {
  $panic('abs_min() arg is an empty sequence');
}
  $j = $x[0];
  $idx = 0;
  while ($idx < count($x)) {
  $i = $x[$idx];
  if (abs_val(floatval($i)) < abs_val(floatval($j))) {
  $j = $i;
}
  $idx = $idx + 1;
};
  return $j;
};
  function abs_max($x) {
  if (count($x) == 0) {
  $panic('abs_max() arg is an empty sequence');
}
  $j = $x[0];
  $idx = 0;
  while ($idx < count($x)) {
  $i = $x[$idx];
  if (abs_val(floatval($i)) > abs_val(floatval($j))) {
  $j = $i;
}
  $idx = $idx + 1;
};
  return $j;
};
  function abs_max_sort($x) {
  if (count($x) == 0) {
  $panic('abs_max_sort() arg is an empty sequence');
}
  $arr = [];
  $i = 0;
  while ($i < count($x)) {
  $arr = _append($arr, $x[$i]);
  $i = $i + 1;
};
  $n = count($arr);
  $a = 0;
  while ($a < $n) {
  $b = 0;
  while ($b < $n - $a - 1) {
  if (abs_val(floatval($arr[$b])) > abs_val(floatval($arr[$b + 1]))) {
  $temp = $arr[$b];
  $arr[$b] = $arr[$b + 1];
  $arr[$b + 1] = $temp;
}
  $b = $b + 1;
};
  $a = $a + 1;
};
  return $arr[$n - 1];
};
  function test_abs_val() {
  if (abs_val(0.0) != 0.0) {
  $panic('abs_val(0) failed');
}
  if (abs_val(34.0) != 34.0) {
  $panic('abs_val(34) failed');
}
  if (abs_val(-100000000000.0) != 100000000000.0) {
  $panic('abs_val large failed');
}
  $a = [-3, -1, 2, -11];
  if (abs_max($a) != (-11)) {
  $panic('abs_max failed');
}
  if (abs_max_sort($a) != (-11)) {
  $panic('abs_max_sort failed');
}
  if (abs_min($a) != (-1)) {
  $panic('abs_min failed');
}
};
  function main() {
  test_abs_val();
  echo rtrim(json_encode(abs_val(-34.0), 1344)), PHP_EOL;
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
