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
  if ($x < 0.0) {
  return 1.0 / expApprox(-$x);
}
  if ($x > 1.0) {
  $half = expApprox($x / 2.0);
  return $half * $half;
}
  $sum = 1.0;
  $term = 1.0;
  $n = 1;
  while ($n < 20) {
  $term = $term * $x / (floatval($n));
  $sum = $sum + $term;
  $n = $n + 1;
};
  return $sum;
};
  function mochi_floor($x) {
  $i = intval($x);
  if ((floatval($i)) > $x) {
  $i = $i - 1;
}
  return floatval($i);
};
  function pow10($n) {
  $result = 1.0;
  $i = 0;
  while ($i < $n) {
  $result = $result * 10.0;
  $i = $i + 1;
};
  return $result;
};
  function mochi_round($x, $n) {
  $m = pow10($n);
  return mochi_floor($x * $m + 0.5) / $m;
};
  function charging_inductor($source_voltage, $resistance, $inductance, $time) {
  if ($source_voltage <= 0.0) {
  _panic('Source voltage must be positive.');
}
  if ($resistance <= 0.0) {
  _panic('Resistance must be positive.');
}
  if ($inductance <= 0.0) {
  _panic('Inductance must be positive.');
}
  $exponent = (-$time * $resistance) / $inductance;
  $current = $source_voltage / $resistance * (1.0 - expApprox($exponent));
  return mochi_round($current, 3);
};
  echo rtrim(json_encode(charging_inductor(5.8, 1.5, 2.3, 2.0), 1344)), PHP_EOL;
  echo rtrim(json_encode(charging_inductor(8.0, 5.0, 3.0, 2.0), 1344)), PHP_EOL;
  echo rtrim(json_encode(charging_inductor(8.0, 500.0, 3.0, 2.0), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
