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
  function astable_frequency($resistance_1, $resistance_2, $capacitance) {
  if ($resistance_1 <= 0.0 || $resistance_2 <= 0.0 || $capacitance <= 0.0) {
  _panic('All values must be positive');
}
  return (1.44 / (($resistance_1 + 2.0 * $resistance_2) * $capacitance)) * 1000000.0;
};
  function astable_duty_cycle($resistance_1, $resistance_2) {
  if ($resistance_1 <= 0.0 || $resistance_2 <= 0.0) {
  _panic('All values must be positive');
}
  return ($resistance_1 + $resistance_2) / ($resistance_1 + 2.0 * $resistance_2) * 100.0;
};
  echo rtrim(json_encode(astable_frequency(45.0, 45.0, 7.0), 1344)), PHP_EOL;
  echo rtrim(json_encode(astable_duty_cycle(45.0, 45.0), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
