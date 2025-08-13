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
  $PI = 3.141592653589793;
  $EQUATORIAL_RADIUS = 6378137.0;
  function to_radians($deg) {
  global $EQUATORIAL_RADIUS, $PI;
  return $deg * $PI / 180.0;
};
  function sin_approx($x) {
  global $EQUATORIAL_RADIUS, $PI;
  $term = $x;
  $sum = $x;
  $i = 1;
  while ($i < 10) {
  $k1 = 2.0 * (floatval($i));
  $k2 = $k1 + 1.0;
  $term = -$term * $x * $x / ($k1 * $k2);
  $sum = $sum + $term;
  $i = $i + 1;
};
  return $sum;
};
  function cos_approx($x) {
  global $EQUATORIAL_RADIUS, $PI;
  $term = 1.0;
  $sum = 1.0;
  $i = 1;
  while ($i < 10) {
  $k1 = 2.0 * (floatval($i)) - 1.0;
  $k2 = 2.0 * (floatval($i));
  $term = -$term * $x * $x / ($k1 * $k2);
  $sum = $sum + $term;
  $i = $i + 1;
};
  return $sum;
};
  function sqrt_approx($x) {
  global $EQUATORIAL_RADIUS, $PI;
  if ($x <= 0.0) {
  return 0.0;
}
  $guess = $x / 2.0;
  $i = 0;
  while ($i < 20) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
};
  function lamberts_ellipsoidal_distance($lat1, $lon1, $lat2, $lon2) {
  global $EQUATORIAL_RADIUS, $PI;
  $phi1 = to_radians($lat1);
  $phi2 = to_radians($lat2);
  $lambda1 = to_radians($lon1);
  $lambda2 = to_radians($lon2);
  $x = ($lambda2 - $lambda1) * cos_approx(($phi1 + $phi2) / 2.0);
  $y = $phi2 - $phi1;
  return $EQUATORIAL_RADIUS * sqrt_approx($x * $x + $y * $y);
};
  echo rtrim(json_encode(lamberts_ellipsoidal_distance(37.774856, -122.424227, 37.864742, -119.537521), 1344)), PHP_EOL;
  echo rtrim(json_encode(lamberts_ellipsoidal_distance(37.774856, -122.424227, 40.713019, -74.012647), 1344)), PHP_EOL;
  echo rtrim(json_encode(lamberts_ellipsoidal_distance(37.774856, -122.424227, 45.443012, 12.313071), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
