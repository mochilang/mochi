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
  function absf($x) {
  if ($x < 0.0) {
  return -$x;
}
  return $x;
};
  function sqrt_approx($x) {
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
  function distance($a, $b) {
  $dx = $b['x'] - $a['x'];
  $dy = $b['y'] - $a['y'];
  $dz = $b['z'] - $a['z'];
  return sqrt_approx(absf($dx * $dx + $dy * $dy + $dz * $dz));
};
  function point_to_string($p) {
  return 'Point(' . _str($p['x']) . ', ' . _str($p['y']) . ', ' . _str($p['z']) . ')';
};
  function test_distance() {
  $p1 = ['x' => 2.0, 'y' => -1.0, 'z' => 7.0];
  $p2 = ['x' => 1.0, 'y' => -3.0, 'z' => 5.0];
  $d = distance($p1, $p2);
  if (absf($d - 3.0) > 0.0001) {
  $panic('distance test failed');
}
  echo rtrim('Distance from ' . point_to_string($p1) . ' to ' . point_to_string($p2) . ' is ' . _str($d)), PHP_EOL;
};
  function main() {
  test_distance();
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
