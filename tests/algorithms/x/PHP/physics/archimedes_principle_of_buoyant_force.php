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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  $G = 9.80665;
  function archimedes_principle($fluid_density, $volume, $gravity) {
  global $G;
  if ($fluid_density <= 0.0) {
  _panic('Impossible fluid density');
}
  if ($volume <= 0.0) {
  _panic('Impossible object volume');
}
  if ($gravity < 0.0) {
  _panic('Impossible gravity');
}
  return $fluid_density * $volume * $gravity;
};
  function archimedes_principle_default($fluid_density, $volume) {
  global $G;
  $res = archimedes_principle($fluid_density, $volume, $G);
  return $res;
};
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
