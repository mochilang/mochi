<?php
error_reporting(E_ALL & ~E_DEPRECATED);
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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$PI = 3.141592653589793;
$TWO_PI = 6.283185307179586;
function _mod($x, $m) {
  global $PI, $TWO_PI;
  return $x - floor($x / $m) * $m;
}
function mochi_cos($x) {
  global $PI, $TWO_PI;
  $y = _mod($x + $PI, $TWO_PI) - $PI;
  $y2 = $y * $y;
  $y4 = $y2 * $y2;
  $y6 = $y4 * $y2;
  return 1.0 - $y2 / 2.0 + $y4 / 24.0 - $y6 / 720.0;
}
function radians($deg) {
  global $PI, $TWO_PI;
  return $deg * $PI / 180.0;
}
function abs_val($x) {
  global $PI, $TWO_PI;
  if ($x < 0.0) {
  return -$x;
}
  return $x;
}
function malus_law($initial_intensity, $angle) {
  global $PI, $TWO_PI;
  if ($initial_intensity < 0.0) {
  _panic('The value of intensity cannot be negative');
}
  if ($angle < 0.0 || $angle > 360.0) {
  _panic('In Malus Law, the angle is in the range 0-360 degrees');
}
  $theta = radians($angle);
  $c = mochi_cos($theta);
  return $initial_intensity * ($c * $c);
}
function main() {
  global $PI, $TWO_PI;
  echo rtrim(_str(malus_law(100.0, 60.0))), PHP_EOL;
}
main();
