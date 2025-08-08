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
  function calculation_span($price) {
  global $spans;
  $n = count($price);
  $st = [];
  $span = [];
  $st = _append($st, 0);
  $span = _append($span, 1);
  for ($i = 1; $i < $n; $i++) {
  while (count($st) > 0 && $price[$st[count($st) - 1]] <= $price[$i]) {
  $st = array_slice($st, 0, count($st) - 1);
};
  $s = (count($st) <= 0 ? $i + 1 : $i - $st[count($st) - 1]);
  $span = _append($span, $s);
  $st = _append($st, $i);
};
  return $span;
};
  function print_array($arr) {
  global $price, $spans;
  for ($i = 0; $i < count($arr); $i++) {
  echo rtrim(json_encode($arr[$i], 1344)), PHP_EOL;
};
};
  $price = [10, 4, 5, 90, 120, 80];
  $spans = calculation_span($price);
  print_array($spans);
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
