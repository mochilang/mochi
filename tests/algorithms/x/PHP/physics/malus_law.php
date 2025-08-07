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
  $PI = 3.141592653589793;
  $TWO_PI = 6.283185307179586;
  function _mod($x, $m) {
  global $PI, $TWO_PI;
  return $x - (floatval(intval($x / $m))) * $m;
};
  function cos($x) {
  global $PI, $TWO_PI;
  $y = _mod($x + $PI, $TWO_PI) - $PI;
  $y2 = $y * $y;
  $y4 = $y2 * $y2;
  $y6 = $y4 * $y2;
  return 1.0 - $y2 / 2.0 + $y4 / 24.0 - $y6 / 720.0;
};
  function radians($deg) {
  global $PI, $TWO_PI;
  return $deg * $PI / 180.0;
};
  function abs_val($x) {
  global $PI, $TWO_PI;
  if ($x < 0.0) {
  return -$x;
}
  return $x;
};
  function malus_law($initial_intensity, $angle) {
  global $PI, $TWO_PI;
  if ($initial_intensity < 0.0) {
  _panic('The value of intensity cannot be negative');
}
  if ($angle < 0.0 || $angle > 360.0) {
  _panic('In Malus Law, the angle is in the range 0-360 degrees');
}
  $theta = radians($angle);
  $c = cos($theta);
  return $initial_intensity * ($c * $c);
};
  function test_malus_law() {
  global $PI, $TWO_PI;
  if (abs_val(malus_law(10.0, 45.0) - 5.0) > 0.01) {
  _panic('malus_law test1 failed');
}
  if (abs_val(malus_law(100.0, 60.0) - 25.0) > 0.01) {
  _panic('malus_law test2 failed');
}
  if (abs_val(malus_law(50.0, 150.0) - 37.5) > 0.01) {
  _panic('malus_law test3 failed');
}
  if (abs_val(malus_law(75.0, 270.0) - 0.0) > 0.01) {
  _panic('malus_law test4 failed');
}
  if (abs_val(malus_law(100.0, 180.0) - 100.0) > 0.01) {
  _panic('malus_law test5 failed');
}
  if (abs_val(malus_law(100.0, 360.0) - 100.0) > 0.01) {
  _panic('malus_law test6 failed');
}
};
  function main() {
  global $PI, $TWO_PI;
  test_malus_law();
  echo rtrim(_str(malus_law(100.0, 60.0))), PHP_EOL;
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
