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
  $TWO_PI = 6.283185307179586;
  function _mod($x, $m) {
  global $PI, $TWO_PI, $forces1, $location1, $forces2, $location2, $forces3, $location3, $forces4, $location4;
  return $x - (floatval(intval($x / $m))) * $m;
};
  function sin_approx($x) {
  global $PI, $TWO_PI, $forces1, $location1, $forces2, $location2, $forces3, $location3, $forces4, $location4;
  $y = _mod($x + $PI, $TWO_PI) - $PI;
  $y2 = $y * $y;
  $y3 = $y2 * $y;
  $y5 = $y3 * $y2;
  $y7 = $y5 * $y2;
  return $y - $y3 / 6.0 + $y5 / 120.0 - $y7 / 5040.0;
};
  function cos_approx($x) {
  global $PI, $TWO_PI, $forces1, $location1, $forces2, $location2, $forces3, $location3, $forces4, $location4;
  $y = _mod($x + $PI, $TWO_PI) - $PI;
  $y2 = $y * $y;
  $y4 = $y2 * $y2;
  $y6 = $y4 * $y2;
  return 1.0 - $y2 / 2.0 + $y4 / 24.0 - $y6 / 720.0;
};
  function polar_force($magnitude, $angle, $radian_mode) {
  global $PI, $TWO_PI, $forces1, $location1, $forces2, $location2, $forces3, $location3, $forces4, $location4;
  $theta = ($radian_mode ? $angle : $angle * $PI / 180.0);
  return [$magnitude * cos_approx($theta), $magnitude * sin_approx($theta)];
};
  function abs_float($x) {
  global $PI, $TWO_PI, $forces1, $location1, $forces2, $location2, $forces3, $location3, $forces4, $location4;
  if ($x < 0.0) {
  return -$x;
} else {
  return $x;
}
};
  function in_static_equilibrium($forces, $location, $eps) {
  global $PI, $TWO_PI, $forces1, $location1, $forces2, $location2, $forces3, $location3, $forces4, $location4;
  $sum_moments = 0.0;
  $i = 0;
  $n = count($forces);
  while ($i < $n) {
  $r = $location[$i];
  $f = $forces[$i];
  $moment = $r[0] * $f[1] - $r[1] * $f[0];
  $sum_moments = $sum_moments + $moment;
  $i = $i + 1;
};
  return abs_float($sum_moments) < $eps;
};
  $forces1 = [[1.0, 1.0], [-1.0, 2.0]];
  $location1 = [[1.0, 0.0], [10.0, 0.0]];
  echo rtrim(_str(in_static_equilibrium($forces1, $location1, 0.1))), PHP_EOL;
  $forces2 = [polar_force(718.4, 150.0, false), polar_force(879.54, 45.0, false), polar_force(100.0, -90.0, false)];
  $location2 = [[0.0, 0.0], [0.0, 0.0], [0.0, 0.0]];
  echo rtrim(_str(in_static_equilibrium($forces2, $location2, 0.1))), PHP_EOL;
  $forces3 = [polar_force(30.0 * 9.81, 15.0, false), polar_force(215.0, 135.0, false), polar_force(264.0, 60.0, false)];
  $location3 = [[0.0, 0.0], [0.0, 0.0], [0.0, 0.0]];
  echo rtrim(_str(in_static_equilibrium($forces3, $location3, 0.1))), PHP_EOL;
  $forces4 = [[0.0, -2000.0], [0.0, -1200.0], [0.0, 15600.0], [0.0, -12400.0]];
  $location4 = [[0.0, 0.0], [6.0, 0.0], [10.0, 0.0], [12.0, 0.0]];
  echo rtrim(_str(in_static_equilibrium($forces4, $location4, 0.1))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
