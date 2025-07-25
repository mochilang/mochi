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
  function angleDiff($b1, $b2) {
  global $testCases;
  $d = $b2 - $b1;
  if ($d < 0 - 180.0) {
  return $d + 360.0;
}
  if ($d > 180.0) {
  return $d - 360.0;
}
  return $d;
};
  $testCases = [[20.0, 45.0], [0 - 45.0, 45.0], [0 - 85.0, 90.0], [0 - 95.0, 90.0], [0 - 45.0, 125.0], [0 - 45.0, 145.0], [29.4803, 0 - 88.6381], [0 - 78.3251, 0 - 159.036]];
  foreach ($testCases as $tc) {
  echo rtrim(json_encode(angleDiff($tc[0], $tc[1]), 1344)), PHP_EOL;
}
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
