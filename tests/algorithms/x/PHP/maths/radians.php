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
$__start_mem = memory_get_usage();
$__start = _now();
  $PI = 3.141592653589793;
  function radians($degree) {
  global $PI;
  return $degree / (180.0 / $PI);
};
  function abs_float($x) {
  global $PI;
  if ($x < 0.0) {
  return -$x;
}
  return $x;
};
  function almost_equal($a, $b) {
  global $PI;
  return abs_float($a - $b) <= 0.00000001;
};
  function test_radians() {
  global $PI;
  if (!almost_equal(radians(180.0), $PI)) {
  $panic('radians 180 failed');
}
  if (!almost_equal(radians(92.0), 1.6057029118347832)) {
  $panic('radians 92 failed');
}
  if (!almost_equal(radians(274.0), 4.782202150464463)) {
  $panic('radians 274 failed');
}
  if (!almost_equal(radians(109.82), 1.9167205845401725)) {
  $panic('radians 109.82 failed');
}
};
  function main() {
  global $PI;
  test_radians();
  echo rtrim(_str(radians(180.0))), PHP_EOL;
  echo rtrim(_str(radians(92.0))), PHP_EOL;
  echo rtrim(_str(radians(274.0))), PHP_EOL;
  echo rtrim(_str(radians(109.82))), PHP_EOL;
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
