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
  function ohms_law($voltage, $current, $resistance) {
  $zeros = 0;
  if ($voltage == 0.0) {
  $zeros = $zeros + 1;
}
  if ($current == 0.0) {
  $zeros = $zeros + 1;
}
  if ($resistance == 0.0) {
  $zeros = $zeros + 1;
}
  if ($zeros != 1) {
  echo rtrim('One and only one argument must be 0'), PHP_EOL;
  return [];
}
  if ($resistance < 0.0) {
  echo rtrim('Resistance cannot be negative'), PHP_EOL;
  return [];
}
  if ($voltage == 0.0) {
  return ['voltage' => $current * $resistance];
}
  if ($current == 0.0) {
  return ['current' => $voltage / $resistance];
}
  return ['resistance' => $voltage / $current];
};
  echo str_replace('    ', '  ', json_encode(ohms_law(10.0, 0.0, 5.0), 128)), PHP_EOL;
  echo str_replace('    ', '  ', json_encode(ohms_law(-10.0, 1.0, 0.0), 128)), PHP_EOL;
  echo str_replace('    ', '  ', json_encode(ohms_law(0.0, -1.5, 2.0), 128)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
