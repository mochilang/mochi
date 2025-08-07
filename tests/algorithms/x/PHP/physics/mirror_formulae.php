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
$__start_mem = memory_get_usage();
$__start = _now();
  function abs_float($x) {
  if ($x < 0.0) {
  return -$x;
}
  return $x;
};
  function isclose($a, $b, $tolerance) {
  return abs_float($a - $b) < $tolerance;
};
  function focal_length($distance_of_object, $distance_of_image) {
  if ($distance_of_object == 0.0 || $distance_of_image == 0.0) {
  _panic('Invalid inputs. Enter non zero values with respect to the sign convention.');
}
  return 1.0 / ((1.0 / $distance_of_object) + (1.0 / $distance_of_image));
};
  function object_distance($focal_length, $distance_of_image) {
  if ($distance_of_image == 0.0 || 'focal_length' == 0.0) {
  _panic('Invalid inputs. Enter non zero values with respect to the sign convention.');
}
  return 1.0 / ((1.0 / 'focal_length') - (1.0 / $distance_of_image));
};
  function image_distance($focal_length, $distance_of_object) {
  if ($distance_of_object == 0.0 || 'focal_length' == 0.0) {
  _panic('Invalid inputs. Enter non zero values with respect to the sign convention.');
}
  return 1.0 / ((1.0 / 'focal_length') - (1.0 / $distance_of_object));
};
  function test_focal_length() {
  $f1 = focal_length(10.0, 20.0);
  if (!isclose($f1, 6.66666666666666, 0.00000001)) {
  _panic('focal_length test1 failed');
}
  $f2 = focal_length(9.5, 6.7);
  if (!isclose($f2, 3.929012346, 0.00000001)) {
  _panic('focal_length test2 failed');
}
};
  function test_object_distance() {
  $u1 = object_distance(30.0, 20.0);
  if (!isclose($u1, -60.0, 0.00000001)) {
  _panic('object_distance test1 failed');
}
  $u2 = object_distance(10.5, 11.7);
  if (!isclose($u2, 102.375, 0.00000001)) {
  _panic('object_distance test2 failed');
}
};
  function test_image_distance() {
  $v1 = image_distance(10.0, 40.0);
  if (!isclose($v1, 13.33333333, 0.00000001)) {
  _panic('image_distance test1 failed');
}
  $v2 = image_distance(1.5, 6.7);
  if (!isclose($v2, 1.932692308, 0.00000001)) {
  _panic('image_distance test2 failed');
}
};
  function main() {
  test_focal_length();
  test_object_distance();
  test_image_distance();
  echo rtrim(_str(focal_length(10.0, 20.0))), PHP_EOL;
  echo rtrim(_str(object_distance(30.0, 20.0))), PHP_EOL;
  echo rtrim(_str(image_distance(10.0, 40.0))), PHP_EOL;
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
