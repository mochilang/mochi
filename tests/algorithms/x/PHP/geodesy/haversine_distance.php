<?php
ini_set('memory_limit', '-1');
function _str($x) {
    if (is_array($x)) {
        $isList = array_keys($x) === range(0, count($x) - 1);
        if ($isList) {
            $parts = [];
            foreach ($x as $v) { $parts[] = _str($v); }
            return '[' . implode(' ', $parts) . ']';
        }
        $parts = [];
        foreach ($x as $k => $v) { $parts[] = _str($k) . ':' . _str($v); }
        return 'map[' . implode(' ', $parts) . ']';
    }
    if (is_bool($x)) return $x ? 'true' : 'false';
    if ($x === null) return 'null';
    return strval($x);
}
$PI = 3.141592653589793;
$AXIS_A = 6378137.0;
$AXIS_B = 6356752.314245;
$RADIUS = 6378137.0;
function to_radians($deg) {
  global $PI, $AXIS_A, $AXIS_B, $RADIUS, $SAN_FRANCISCO, $YOSEMITE;
  return $deg * $PI / 180.0;
}
function sin_taylor($x) {
  global $PI, $AXIS_A, $AXIS_B, $RADIUS, $SAN_FRANCISCO, $YOSEMITE;
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
function cos_taylor($x) {
  global $PI, $AXIS_A, $AXIS_B, $RADIUS, $SAN_FRANCISCO, $YOSEMITE;
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
function tan_approx($x) {
  global $PI, $AXIS_A, $AXIS_B, $RADIUS, $SAN_FRANCISCO, $YOSEMITE;
  return sin_taylor($x) / cos_taylor($x);
}
function sqrtApprox($x) {
  global $PI, $AXIS_A, $AXIS_B, $RADIUS, $SAN_FRANCISCO, $YOSEMITE;
  $guess = $x / 2.0;
  $i = 0;
  while ($i < 20) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
}
function atanApprox($x) {
  global $PI, $AXIS_A, $AXIS_B, $RADIUS, $SAN_FRANCISCO, $YOSEMITE;
  if ($x > 1.0) {
  return $PI / 2.0 - $x / ($x * $x + 0.28);
}
  if ($x < (-1.0)) {
  return -$PI / 2.0 - $x / ($x * $x + 0.28);
}
  return $x / (1.0 + 0.28 * $x * $x);
}
function atan2Approx($y, $x) {
  global $PI, $AXIS_A, $AXIS_B, $RADIUS, $SAN_FRANCISCO, $YOSEMITE;
  if ($x > 0.0) {
  $val = atanApprox($y / $x);
  return $val;
}
  if ($x < 0.0) {
  if ($y >= 0.0) {
  return atanApprox($y / $x) + $PI;
};
  return atanApprox($y / $x) - $PI;
}
  if ($y > 0.0) {
  return $PI / 2.0;
}
  if ($y < 0.0) {
  return -$PI / 2.0;
}
  return 0.0;
}
function asinApprox($x) {
  global $PI, $AXIS_A, $AXIS_B, $RADIUS, $SAN_FRANCISCO, $YOSEMITE;
  $denom = sqrtApprox(1.0 - $x * $x);
  $res = atan2Approx($x, $denom);
  return $res;
}
function haversine_distance($lat1, $lon1, $lat2, $lon2) {
  global $PI, $AXIS_A, $AXIS_B, $RADIUS, $SAN_FRANCISCO, $YOSEMITE;
  $flattening = ($AXIS_A - $AXIS_B) / $AXIS_A;
  $phi_1 = atanApprox((1.0 - $flattening) * tan_approx(to_radians($lat1)));
  $phi_2 = atanApprox((1.0 - $flattening) * tan_approx(to_radians($lat2)));
  $lambda_1 = to_radians($lon1);
  $lambda_2 = to_radians($lon2);
  $sin_sq_phi = sin_taylor(($phi_2 - $phi_1) / 2.0);
  $sin_sq_lambda = sin_taylor(($lambda_2 - $lambda_1) / 2.0);
  $sin_sq_phi = $sin_sq_phi * $sin_sq_phi;
  $sin_sq_lambda = $sin_sq_lambda * $sin_sq_lambda;
  $h_value = sqrtApprox($sin_sq_phi + cos_taylor($phi_1) * cos_taylor($phi_2) * $sin_sq_lambda);
  return 2.0 * $RADIUS * asinApprox($h_value);
}
$SAN_FRANCISCO = [37.774856, -122.424227];
$YOSEMITE = [37.864742, -119.537521];
echo rtrim(_str(haversine_distance($SAN_FRANCISCO[0], $SAN_FRANCISCO[1], $YOSEMITE[0], $YOSEMITE[1]))), PHP_EOL;
