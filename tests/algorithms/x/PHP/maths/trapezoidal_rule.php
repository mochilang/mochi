<?php
error_reporting(E_ALL & ~E_DEPRECATED);
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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function f($x) {
  global $a, $b, $boundary, $steps, $y;
  return $x * $x;
};
  function make_points($a, $b, $h) {
  global $boundary, $steps, $y;
  $xs = [];
  $x = $a + $h;
  while ($x <= ($b - $h)) {
  $xs = _append($xs, $x);
  $x = $x + $h;
};
  return $xs;
};
  function trapezoidal_rule($boundary, $steps) {
  $h = ($boundary[1] - $boundary[0]) / $steps;
  $a = $boundary[0];
  $b = $boundary[1];
  $xs = make_points($a, $b, $h);
  $y = ($h / 2.0) * f($a);
  $i = 0;
  while ($i < count($xs)) {
  $y = $y + $h * f($xs[$i]);
  $i = $i + 1;
};
  $y = $y + ($h / 2.0) * f($b);
  return $y;
};
  $a = 0.0;
  $b = 1.0;
  $steps = 10.0;
  $boundary = [$a, $b];
  $y = trapezoidal_rule($boundary, $steps);
  echo rtrim('y = ' . _str($y)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
