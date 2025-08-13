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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  $PI = 3.141592653589793;
  function make_angle($deg) {
  global $PI;
  if ($deg < 0.0 || $deg > 360.0) {
  _panic('degrees must be between 0 and 360');
}
  return ['degrees' => $deg];
};
  function make_side($length, $angle) {
  global $PI;
  if ($length <= 0.0) {
  _panic('length must be positive');
}
  return ['length' => $length, 'angle' => $angle, 'next' => -1];
};
  function ellipse_area($e) {
  global $PI;
  return $PI * $e['major'] * $e['minor'];
};
  function ellipse_perimeter($e) {
  global $PI;
  return $PI * ($e['major'] + $e['minor']);
};
  function circle_area($c) {
  global $PI;
  $e = ['major' => $c['radius'], 'minor' => $c['radius']];
  $area = ellipse_area($e);
  return $area;
};
  function circle_perimeter($c) {
  global $PI;
  $e = ['major' => $c['radius'], 'minor' => $c['radius']];
  $per = ellipse_perimeter($e);
  return $per;
};
  function circle_diameter($c) {
  global $PI;
  return $c['radius'] * 2.0;
};
  function circle_max_parts($num_cuts) {
  global $PI;
  if ($num_cuts < 0.0) {
  _panic('num_cuts must be positive');
}
  return ($num_cuts + 2.0 + $num_cuts * $num_cuts) * 0.5;
};
  function make_polygon() {
  global $PI;
  $s = [];
  return ['sides' => $s];
};
  function polygon_add_side(&$p, $s) {
  global $PI;
  $p['sides'] = _append($p['sides'], $s);
};
  function polygon_get_side($p, $index) {
  global $PI;
  return $p['sides'][$index];
};
  function polygon_set_side(&$p, $index, $s) {
  global $PI;
  $tmp = $p['sides'];
  $tmp[$index] = $s;
  $p['sides'] = $tmp;
};
  function make_rectangle($short_len, $long_len) {
  global $PI;
  if ($short_len <= 0.0 || $long_len <= 0.0) {
  _panic('length must be positive');
}
  $short = make_side($short_len, make_angle(90.0));
  $long = make_side($long_len, make_angle(90.0));
  $p = make_polygon();
  polygon_add_side($p, $short);
  polygon_add_side($p, $long);
  return ['short_side' => $short, 'long_side' => $long, 'poly' => $p];
};
  function rectangle_perimeter($r) {
  global $PI;
  return ($r['short_side']['length'] + $r['long_side']['length']) * 2.0;
};
  function rectangle_area($r) {
  global $PI;
  return $r['short_side']['length'] * $r['long_side']['length'];
};
  function make_square($side_len) {
  global $PI;
  $rect = make_rectangle($side_len, $side_len);
  return ['side' => $rect['short_side'], 'rect' => $rect];
};
  function square_perimeter($s) {
  global $PI;
  $p = rectangle_perimeter($s['rect']);
  return $p;
};
  function square_area($s) {
  global $PI;
  $a = rectangle_area($s['rect']);
  return $a;
};
  function main() {
  global $PI;
  $a = make_angle(90.0);
  echo rtrim(json_encode($a['degrees'], 1344)), PHP_EOL;
  $s = make_side(5.0, $a);
  echo rtrim(json_encode($s['length'], 1344)), PHP_EOL;
  $e = ['major' => 5.0, 'minor' => 10.0];
  echo rtrim(json_encode(ellipse_area($e), 1344)), PHP_EOL;
  echo rtrim(json_encode(ellipse_perimeter($e), 1344)), PHP_EOL;
  $c = ['radius' => 5.0];
  echo rtrim(json_encode(circle_area($c), 1344)), PHP_EOL;
  echo rtrim(json_encode(circle_perimeter($c), 1344)), PHP_EOL;
  echo rtrim(json_encode(circle_diameter($c), 1344)), PHP_EOL;
  echo rtrim(json_encode(circle_max_parts(7.0), 1344)), PHP_EOL;
  $r = make_rectangle(5.0, 10.0);
  echo rtrim(json_encode(rectangle_perimeter($r), 1344)), PHP_EOL;
  echo rtrim(json_encode(rectangle_area($r), 1344)), PHP_EOL;
  $q = make_square(5.0);
  echo rtrim(json_encode(square_perimeter($q), 1344)), PHP_EOL;
  echo rtrim(json_encode(square_area($q), 1344)), PHP_EOL;
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
