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
  function sqrtApprox($x) {
  $guess = $x / 2.0;
  $i = 0;
  while ($i < 20) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
};
  function abs_val($num) {
  if ($num < 0.0) {
  return -$num;
}
  return $num;
};
  function approx_equal($a, $b, $eps) {
  return abs_val($a - $b) < $eps;
};
  function dodecahedron_surface_area($edge) {
  if ($edge <= 0) {
  $panic('Length must be a positive.');
}
  $term = sqrtApprox(25.0 + 10.0 * sqrtApprox(5.0));
  $e = floatval($edge);
  return 3.0 * $term * $e * $e;
};
  function dodecahedron_volume($edge) {
  if ($edge <= 0) {
  $panic('Length must be a positive.');
}
  $term = (15.0 + 7.0 * sqrtApprox(5.0)) / 4.0;
  $e = floatval($edge);
  return $term * $e * $e * $e;
};
  function test_dodecahedron() {
  if (!approx_equal(dodecahedron_surface_area(5), 516.1432201766901, 0.0001)) {
  $panic('surface area 5 failed');
}
  if (!approx_equal(dodecahedron_surface_area(10), 2064.5728807067603, 0.0001)) {
  $panic('surface area 10 failed');
}
  if (!approx_equal(dodecahedron_volume(5), 957.8898700780791, 0.0001)) {
  $panic('volume 5 failed');
}
  if (!approx_equal(dodecahedron_volume(10), 7663.118960624633, 0.0001)) {
  $panic('volume 10 failed');
}
};
  function main() {
  test_dodecahedron();
  echo rtrim(json_encode(dodecahedron_surface_area(5), 1344)), PHP_EOL;
  echo rtrim(json_encode(dodecahedron_volume(5), 1344)), PHP_EOL;
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
