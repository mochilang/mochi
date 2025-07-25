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
  $diff = $b2 - $b1;
  return (fmod((fmod($diff, 360.0) + 360.0 + 180.0), 360.0)) - 180.0;
};
  $testCases = [[20.0, 45.0], [0 - 45.0, 45.0], [0 - 85.0, 90.0], [0 - 95.0, 90.0], [0 - 45.0, 125.0], [0 - 45.0, 145.0], [29.4803, 0 - 88.6381], [0 - 78.3251, 0 - 159.036], [0 - 70099.74233810938, 29840.67437876723], [0 - 165313.6666297357, 33693.9894517456], [1174.8380510598456, 0 - 154146.66490124757], [60175.77306795546, 42213.07192354373]];
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
