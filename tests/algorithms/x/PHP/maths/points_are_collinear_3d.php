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
  function create_vector($p1, $p2) {
  $vx = $p2['x'] - $p1['x'];
  $vy = $p2['y'] - $p1['y'];
  $vz = $p2['z'] - $p1['z'];
  return ['x' => $vx, 'y' => $vy, 'z' => $vz];
};
  function get_3d_vectors_cross($ab, $ac) {
  $cx = $ab['y'] * $ac['z'] - $ab['z'] * $ac['y'];
  $cy = $ab['z'] * $ac['x'] - $ab['x'] * $ac['z'];
  $cz = $ab['x'] * $ac['y'] - $ab['y'] * $ac['x'];
  return ['x' => $cx, 'y' => $cy, 'z' => $cz];
};
  function pow10($exp) {
  $result = 1.0;
  $i = 0;
  while ($i < $exp) {
  $result = $result * 10.0;
  $i = $i + 1;
};
  return $result;
};
  function round_float($x, $digits) {
  $factor = pow10($digits);
  $v = $x * $factor;
  if ($v >= 0.0) {
  $v = $v + 0.5;
} else {
  $v = $v - 0.5;
}
  $t = intval($v);
  return (floatval($t)) / $factor;
};
  function is_zero_vector($v, $accuracy) {
  return round_float($v['x'], $accuracy) == 0.0 && round_float($v['y'], $accuracy) == 0.0 && round_float($v['z'], $accuracy) == 0.0;
};
  function are_collinear($a, $b, $c, $accuracy) {
  $ab = create_vector($a, $b);
  $ac = create_vector($a, $c);
  $cross = get_3d_vectors_cross($ab, $ac);
  return is_zero_vector($cross, $accuracy);
};
  function test_are_collinear() {
  $p1 = ['x' => 0.0, 'y' => 0.0, 'z' => 0.0];
  $p2 = ['x' => 1.0, 'y' => 1.0, 'z' => 1.0];
  $p3 = ['x' => 2.0, 'y' => 2.0, 'z' => 2.0];
  if (!are_collinear($p1, $p2, $p3, 10)) {
  $panic('collinear test failed');
}
  $q3 = ['x' => 1.0, 'y' => 2.0, 'z' => 3.0];
  if (are_collinear($p1, $p2, $q3, 10)) {
  $panic('non-collinear test failed');
}
};
  function main() {
  test_are_collinear();
  $a = ['x' => 4.802293498137402, 'y' => 3.536233125455244, 'z' => 0.0];
  $b = ['x' => -2.186788107953106, 'y' => -9.24561398001649, 'z' => 7.141509524846482];
  $c = ['x' => 1.530169574640268, 'y' => -2.447927606600034, 'z' => 3.343487096469054];
  echo rtrim(_str(are_collinear($a, $b, $c, 10))), PHP_EOL;
  $d = ['x' => 2.399001826862445, 'y' => -2.452009976680793, 'z' => 4.464656666157666];
  $e = ['x' => -3.682816335934376, 'y' => 5.753788986533145, 'z' => 9.490993909044244];
  $f = ['x' => 1.962903518985307, 'y' => 3.741415730125627, 'z' => 7.0];
  echo rtrim(_str(are_collinear($d, $e, $f, 10))), PHP_EOL;
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
