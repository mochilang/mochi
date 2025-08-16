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
  function insertion_sort($a) {
  $i = 1;
  while ($i < count($a)) {
  $key = $a[$i];
  $j = $i - 1;
  while ($j >= 0 && $a[$j] > $key) {
  $a[$j + 1] = $a[$j];
  $j = $j - 1;
};
  $a[$j + 1] = $key;
  $i = $i + 1;
};
  return $a;
};
  function minimum_waiting_time($queries) {
  $n = count($queries);
  if ($n == 0 || $n == 1) {
  return 0;
}
  $sorted = insertion_sort($queries);
  $total = 0;
  $i = 0;
  while ($i < $n) {
  $total = $total + $sorted[$i] * ($n - $i - 1);
  $i = $i + 1;
};
  return $total;
};
  echo rtrim(json_encode(minimum_waiting_time([3, 2, 1, 2, 6]), 1344)), PHP_EOL;
  echo rtrim(json_encode(minimum_waiting_time([3, 2, 1]), 1344)), PHP_EOL;
  echo rtrim(json_encode(minimum_waiting_time([1, 2, 3, 4]), 1344)), PHP_EOL;
  echo rtrim(json_encode(minimum_waiting_time([5, 5, 5, 5]), 1344)), PHP_EOL;
  echo rtrim(json_encode(minimum_waiting_time([]), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
