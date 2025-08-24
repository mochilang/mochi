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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  $seed = 1;
  function mochi_rand() {
  global $pts, $seed;
  $seed = ($seed * 1103515245 + 12345) % 2147483648;
  return $seed;
};
  function random() {
  global $pts, $seed;
  return (floatval(mochi_rand())) / 2147483648.0;
};
  function hypercube_points($num_points, $hypercube_size, $num_dimensions) {
  global $pts, $seed;
  $points = [];
  $i = 0;
  while ($i < $num_points) {
  $point = [];
  $j = 0;
  while ($j < $num_dimensions) {
  $value = $hypercube_size * random();
  $point = _append($point, $value);
  $j = $j + 1;
};
  $points = _append($points, $point);
  $i = $i + 1;
};
  return $points;
};
  $pts = hypercube_points(3, 1.0, 2);
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($pts, 1344)))))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
