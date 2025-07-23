<?php
function d2d($d) {
  global $g2g, $m2m, $r2r, $d2g, $d2m, $d2r, $g2d, $g2m, $g2r, $m2d, $m2g, $m2r, $r2d, $r2g, $r2m, $main;
  return $d % 360.0;
}
function g2g($g) {
  global $d2d, $m2m, $r2r, $d2g, $d2m, $d2r, $g2d, $g2m, $g2r, $m2d, $m2g, $m2r, $r2d, $r2g, $r2m, $main;
  return $g % 400.0;
}
function m2m($m) {
  global $d2d, $g2g, $r2r, $d2g, $d2m, $d2r, $g2d, $g2m, $g2r, $m2d, $m2g, $m2r, $r2d, $r2g, $r2m, $main;
  return $m % 6400.0;
}
function r2r($r) {
  global $d2d, $g2g, $m2m, $d2g, $d2m, $d2r, $g2d, $g2m, $g2r, $m2d, $m2g, $m2r, $r2d, $r2g, $r2m, $main;
  return $r % (2.0 * 3.141592653589793);
}
function d2g($d) {
  global $d2d, $g2g, $m2m, $r2r, $d2m, $d2r, $g2d, $g2m, $g2r, $m2d, $m2g, $m2r, $r2d, $r2g, $r2m, $main;
  return d2d($d) * 400.0 / 360.0;
}
function d2m($d) {
  global $d2d, $g2g, $m2m, $r2r, $d2g, $d2r, $g2d, $g2m, $g2r, $m2d, $m2g, $m2r, $r2d, $r2g, $r2m, $main;
  return d2d($d) * 6400.0 / 360.0;
}
function d2r($d) {
  global $d2d, $g2g, $m2m, $r2r, $d2g, $d2m, $g2d, $g2m, $g2r, $m2d, $m2g, $m2r, $r2d, $r2g, $r2m, $main;
  return d2d($d) * 3.141592653589793 / 180.0;
}
function g2d($g) {
  global $d2d, $g2g, $m2m, $r2r, $d2g, $d2m, $d2r, $g2m, $g2r, $m2d, $m2g, $m2r, $r2d, $r2g, $r2m, $main;
  return g2g($g) * 360.0 / 400.0;
}
function g2m($g) {
  global $d2d, $g2g, $m2m, $r2r, $d2g, $d2m, $d2r, $g2d, $g2r, $m2d, $m2g, $m2r, $r2d, $r2g, $r2m, $main;
  return g2g($g) * 6400.0 / 400.0;
}
function g2r($g) {
  global $d2d, $g2g, $m2m, $r2r, $d2g, $d2m, $d2r, $g2d, $g2m, $m2d, $m2g, $m2r, $r2d, $r2g, $r2m, $main;
  return g2g($g) * 3.141592653589793 / 200.0;
}
function m2d($m) {
  global $d2d, $g2g, $m2m, $r2r, $d2g, $d2m, $d2r, $g2d, $g2m, $g2r, $m2g, $m2r, $r2d, $r2g, $r2m, $main;
  return m2m($m) * 360.0 / 6400.0;
}
function m2g($m) {
  global $d2d, $g2g, $m2m, $r2r, $d2g, $d2m, $d2r, $g2d, $g2m, $g2r, $m2d, $m2r, $r2d, $r2g, $r2m, $main;
  return m2m($m) * 400.0 / 6400.0;
}
function m2r($m) {
  global $d2d, $g2g, $m2m, $r2r, $d2g, $d2m, $d2r, $g2d, $g2m, $g2r, $m2d, $m2g, $r2d, $r2g, $r2m, $main;
  return m2m($m) * 3.141592653589793 / 3200.0;
}
function r2d($r) {
  global $d2d, $g2g, $m2m, $r2r, $d2g, $d2m, $d2r, $g2d, $g2m, $g2r, $m2d, $m2g, $m2r, $r2g, $r2m, $main;
  return r2r($r) * 180.0 / 3.141592653589793;
}
function r2g($r) {
  global $d2d, $g2g, $m2m, $r2r, $d2g, $d2m, $d2r, $g2d, $g2m, $g2r, $m2d, $m2g, $m2r, $r2d, $r2m, $main;
  return r2r($r) * 200.0 / 3.141592653589793;
}
function r2m($r) {
  global $d2d, $g2g, $m2m, $r2r, $d2g, $d2m, $d2r, $g2d, $g2m, $g2r, $m2d, $m2g, $m2r, $r2d, $r2g, $main;
  return r2r($r) * 3200.0 / 3.141592653589793;
}
function main() {
  global $d2d, $g2g, $m2m, $r2r, $d2g, $d2m, $d2r, $g2d, $g2m, $g2r, $m2d, $m2g, $m2r, $r2d, $r2g, $r2m;
  $angles = [-2.0, -1.0, 0.0, 1.0, 2.0, 6.2831853, 16.0, 57.2957795, 359.0, 399.0, 6399.0, 1000000.0];
  echo "degrees normalized_degs gradians mils radians", PHP_EOL;
  foreach ($angles as $a) {
  echo json_encode($a, 1344) . " " . json_encode(d2d($a), 1344) . " " . json_encode(d2g($a), 1344) . " " . json_encode(d2m($a), 1344) . " " . json_encode(d2r($a), 1344), PHP_EOL;
};
  echo "\ngradians normalized_grds degrees mils radians", PHP_EOL;
  foreach ($angles as $a) {
  echo json_encode($a, 1344) . " " . json_encode(g2g($a), 1344) . " " . json_encode(g2d($a), 1344) . " " . json_encode(g2m($a), 1344) . " " . json_encode(g2r($a), 1344), PHP_EOL;
};
  echo "\nmils normalized_mils degrees gradians radians", PHP_EOL;
  foreach ($angles as $a) {
  echo json_encode($a, 1344) . " " . json_encode(m2m($a), 1344) . " " . json_encode(m2d($a), 1344) . " " . json_encode(m2g($a), 1344) . " " . json_encode(m2r($a), 1344), PHP_EOL;
};
  echo "\nradians normalized_rads degrees gradians mils", PHP_EOL;
  foreach ($angles as $a) {
  echo json_encode($a, 1344) . " " . json_encode(r2r($a), 1344) . " " . json_encode(r2d($a), 1344) . " " . json_encode(r2g($a), 1344) . " " . json_encode(r2m($a), 1344), PHP_EOL;
};
}
main();
