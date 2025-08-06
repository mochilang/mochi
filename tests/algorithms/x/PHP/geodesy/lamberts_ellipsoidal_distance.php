<?php
ini_set('memory_limit', '-1');
$PI = 3.141592653589793;
$EQUATORIAL_RADIUS = 6378137.0;
function to_radians($deg) {
  global $PI, $EQUATORIAL_RADIUS;
  return $deg * $PI / 180.0;
}
function sin_approx($x) {
  global $PI, $EQUATORIAL_RADIUS;
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
}
function cos_approx($x) {
  global $PI, $EQUATORIAL_RADIUS;
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
}
function sqrt_approx($x) {
  global $PI, $EQUATORIAL_RADIUS;
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
}
function lamberts_ellipsoidal_distance($lat1, $lon1, $lat2, $lon2) {
  global $PI, $EQUATORIAL_RADIUS;
  $phi1 = to_radians($lat1);
  $phi2 = to_radians($lat2);
  $lambda1 = to_radians($lon1);
  $lambda2 = to_radians($lon2);
  $x = ($lambda2 - $lambda1) * cos_approx(($phi1 + $phi2) / 2.0);
  $y = $phi2 - $phi1;
  return $EQUATORIAL_RADIUS * sqrt_approx($x * $x + $y * $y);
}
echo rtrim(json_encode(lamberts_ellipsoidal_distance(37.774856, -122.424227, 37.864742, -119.537521), 1344)), PHP_EOL;
echo rtrim(json_encode(lamberts_ellipsoidal_distance(37.774856, -122.424227, 40.713019, -74.012647), 1344)), PHP_EOL;
echo rtrim(json_encode(lamberts_ellipsoidal_distance(37.774856, -122.424227, 45.443012, 12.313071), 1344)), PHP_EOL;
