<?php
error_reporting(E_ALL & ~E_DEPRECATED);
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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function fabs($x) {
  if ($x < 0.0) {
  return -$x;
} else {
  return $x;
}
};
  function reynolds_number($density, $velocity, $diameter, $viscosity) {
  if ($density <= 0.0 || $diameter <= 0.0 || $viscosity <= 0.0) {
  _panic('please ensure that density, diameter and viscosity are positive');
}
  return ($density * fabs($velocity) * $diameter) / $viscosity;
};
  echo rtrim(json_encode(reynolds_number(900.0, 2.5, 0.05, 0.4), 1344)), PHP_EOL;
  echo rtrim(json_encode(reynolds_number(450.0, 3.86, 0.078, 0.23), 1344)), PHP_EOL;
  echo rtrim(json_encode(reynolds_number(234.0, -4.5, 0.3, 0.44), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
