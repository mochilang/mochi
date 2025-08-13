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
  function expApprox($x) {
  $y = $x;
  $is_neg = false;
  if ($x < 0.0) {
  $is_neg = true;
  $y = -$x;
}
  $term = 1.0;
  $sum = 1.0;
  $n = 1;
  while ($n < 30) {
  $term = $term * $y / (floatval($n));
  $sum = $sum + $term;
  $n = $n + 1;
};
  if ($is_neg) {
  return 1.0 / $sum;
}
  return $sum;
};
  function round3($x) {
  $scaled = $x * 1000.0;
  if ($scaled >= 0.0) {
  $scaled = $scaled + 0.5;
} else {
  $scaled = $scaled - 0.5;
}
  $scaled_int = intval($scaled);
  return (floatval($scaled_int)) / 1000.0;
};
  function charging_capacitor($source_voltage, $resistance, $capacitance, $time_sec) {
  if ($source_voltage <= 0.0) {
  _panic('Source voltage must be positive.');
}
  if ($resistance <= 0.0) {
  _panic('Resistance must be positive.');
}
  if ($capacitance <= 0.0) {
  _panic('Capacitance must be positive.');
}
  $exponent = -$time_sec / ($resistance * $capacitance);
  $voltage = $source_voltage * (1.0 - expApprox($exponent));
  return round3($voltage);
};
  echo rtrim(json_encode(charging_capacitor(0.2, 0.9, 8.4, 0.5), 1344)), PHP_EOL;
  echo rtrim(json_encode(charging_capacitor(2.2, 3.5, 2.4, 9.0), 1344)), PHP_EOL;
  echo rtrim(json_encode(charging_capacitor(15.0, 200.0, 20.0, 2.0), 1344)), PHP_EOL;
  echo rtrim(json_encode(charging_capacitor(20.0, 2000.0, 0.0003, 4.0), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
