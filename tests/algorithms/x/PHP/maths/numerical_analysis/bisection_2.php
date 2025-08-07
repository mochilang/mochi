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
  function equation($x) {
  return 10.0 - $x * $x;
};
  function bisection($a, $b) {
  if (equation($a) * equation($b) >= 0.0) {
  $panic('Wrong space!');
}
  $left = $a;
  $right = $b;
  $c = $left;
  while (($right - $left) >= 0.01) {
  $c = ($left + $right) / 2.0;
  if (equation($c) == 0.0) {
  break;
}
  if (equation($c) * equation($left) < 0.0) {
  $right = $c;
} else {
  $left = $c;
}
};
  return $c;
};
  echo rtrim(json_encode(bisection(-2.0, 5.0), 1344)), PHP_EOL;
  echo rtrim(json_encode(bisection(0.0, 6.0), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
