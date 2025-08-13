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
  $PI = 3.141592653589793;
  function ind_reactance($inductance, $frequency, $reactance) {
  global $PI;
  $zero_count = 0;
  if ($inductance == 0.0) {
  $zero_count = $zero_count + 1;
}
  if ($frequency == 0.0) {
  $zero_count = $zero_count + 1;
}
  if ($reactance == 0.0) {
  $zero_count = $zero_count + 1;
}
  if ($zero_count != 1) {
  _panic('One and only one argument must be 0');
}
  if ($inductance < 0.0) {
  _panic('Inductance cannot be negative');
}
  if ($frequency < 0.0) {
  _panic('Frequency cannot be negative');
}
  if ($reactance < 0.0) {
  _panic('Inductive reactance cannot be negative');
}
  if ($inductance == 0.0) {
  return ['inductance' => $reactance / (2.0 * $PI * $frequency)];
}
  if ($frequency == 0.0) {
  return ['frequency' => $reactance / (2.0 * $PI * $inductance)];
}
  return ['reactance' => 2.0 * $PI * $frequency * $inductance];
};
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(ind_reactance(0.0, 10000.0, 50.0), 1344)))))), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(ind_reactance(0.035, 0.0, 50.0), 1344)))))), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(ind_reactance(0.000035, 1000.0, 0.0), 1344)))))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
