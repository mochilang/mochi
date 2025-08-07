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
  function is_geometric_series($series) {
  if (count($series) == 0) {
  $panic('Input list must be a non empty list');
}
  if (count($series) == 1) {
  return true;
}
  if ($series[0] == 0.0) {
  return false;
}
  $ratio = $series[1] / $series[0];
  $i = 0;
  while ($i < count($series) - 1) {
  if ($series[$i] == 0.0) {
  return false;
}
  if ($series[$i + 1] / $series[$i] != $ratio) {
  return false;
}
  $i = $i + 1;
};
  return true;
};
  function geometric_mean($series) {
  if (count($series) == 0) {
  $panic('Input list must be a non empty list');
}
  $product = 1.0;
  $i = 0;
  while ($i < count($series)) {
  $product = $product * $series[$i];
  $i = $i + 1;
};
  $n = count($series);
  return nth_root($product, $n);
};
  function pow_float($base, $exp) {
  $result = 1.0;
  $i = 0;
  while ($i < $exp) {
  $result = $result * $base;
  $i = $i + 1;
};
  return $result;
};
  function nth_root($value, $n) {
  if ($value == 0.0) {
  return 0.0;
}
  $low = 0.0;
  $high = $value;
  if ($value < 1.0) {
  $high = 1.0;
}
  $mid = ($low + $high) / 2.0;
  $i = 0;
  while ($i < 40) {
  $mp = pow_float($mid, $n);
  if ($mp > $value) {
  $high = $mid;
} else {
  $low = $mid;
}
  $mid = ($low + $high) / 2.0;
  $i = $i + 1;
};
  return $mid;
};
  function test_geometric() {
  $a = [2.0, 4.0, 8.0];
  if (!is_geometric_series($a)) {
  $panic('expected geometric series');
}
  $b = [1.0, 2.0, 3.0];
  if (is_geometric_series($b)) {
  $panic('expected non geometric series');
}
};
  function main() {
  test_geometric();
  echo rtrim(json_encode(geometric_mean([2.0, 4.0, 8.0]), 1344)), PHP_EOL;
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
