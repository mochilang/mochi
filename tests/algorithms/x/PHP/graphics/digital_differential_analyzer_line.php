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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function abs_int($n) {
  if ($n < 0) {
  return -$n;
}
  return $n;
};
  function round_int($x) {
  return intval(($x + 0.5));
};
  function digital_differential_analyzer_line($p1, $p2) {
  $dx = $p2['x'] - $p1['x'];
  $dy = $p2['y'] - $p1['y'];
  $abs_dx = abs_int($dx);
  $abs_dy = abs_int($dy);
  $steps = ($abs_dx > $abs_dy ? $abs_dx : $abs_dy);
  $x_increment = (floatval($dx)) / (floatval($steps));
  $y_increment = (floatval($dy)) / (floatval($steps));
  $coordinates = [];
  $x = floatval($p1['x']);
  $y = floatval($p1['y']);
  $i = 0;
  while ($i < $steps) {
  $x = $x + $x_increment;
  $y = $y + $y_increment;
  $point = ['x' => round_int($x), 'y' => round_int($y)];
  $coordinates = _append($coordinates, $point);
  $i = $i + 1;
};
  return $coordinates;
};
  function main() {
  $result = digital_differential_analyzer_line(['x' => 1, 'y' => 1], ['x' => 4, 'y' => 4]);
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($result, 1344)))))), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
