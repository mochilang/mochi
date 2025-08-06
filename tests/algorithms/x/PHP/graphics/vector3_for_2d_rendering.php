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
function mochi_floor($x) {
  global $PI;
  $i = intval($x);
  if ((floatval($i)) > $x) {
  $i = $i - 1;
}
  return floatval($i);
}
function modf($x, $m) {
  global $PI;
  return $x - mochi_floor($x / $m) * $m;
}
function sin_taylor($x) {
  global $PI;
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
  global $PI;
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
function convert_to_2d($x, $y, $z, $scale, $distance) {
  global $PI;
  $projected_x = (($x * $distance) / ($z + $distance)) * $scale;
  $projected_y = (($y * $distance) / ($z + $distance)) * $scale;
  return [$projected_x, $projected_y];
}
function rotate($x, $y, $z, $axis, $angle) {
  global $PI;
  $angle = modf($angle, 360.0) / 450.0 * 180.0 / $PI;
  $angle = modf($angle, 2.0 * $PI);
  if ($angle > $PI) {
  $angle = $angle - 2.0 * $PI;
}
  if ($axis == 'z') {
  $new_x = $x * cos_taylor($angle) - $y * sin_taylor($angle);
  $new_y = $y * cos_taylor($angle) + $x * sin_taylor($angle);
  $new_z = $z;
  return [$new_x, $new_y, $new_z];
}
  if ($axis == 'x') {
  $new_y = $y * cos_taylor($angle) - $z * sin_taylor($angle);
  $new_z = $z * cos_taylor($angle) + $y * sin_taylor($angle);
  $new_x = $x;
  return [$new_x, $new_y, $new_z];
}
  if ($axis == 'y') {
  $new_x = $x * cos_taylor($angle) - $z * sin_taylor($angle);
  $new_z = $z * cos_taylor($angle) + $x * sin_taylor($angle);
  $new_y = $y;
  return [$new_x, $new_y, $new_z];
}
  echo rtrim('not a valid axis, choose one of \'x\', \'y\', \'z\''), PHP_EOL;
  return [0.0, 0.0, 0.0];
}
echo rtrim(_str(convert_to_2d(1.0, 2.0, 3.0, 10.0, 10.0))), PHP_EOL;
echo rtrim(_str(rotate(1.0, 2.0, 3.0, 'y', 90.0))), PHP_EOL;
