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
  function sinApprox($x) {
  global $PI, $degreesIncr, $turns, $stop, $width, $centre, $a, $b, $theta, $count, $r, $y;
  $term = $x;
  $sum = $x;
  $n = 1;
  while ($n <= 10) {
  $denom = floatval(((2 * $n) * (2 * $n + 1)));
  $term = -$term * $x * $x / $denom;
  $sum = $sum + $term;
  $n = $n + 1;
};
  return $sum;
};
  function cosApprox($x) {
  global $PI, $degreesIncr, $turns, $stop, $width, $centre, $a, $b, $theta, $count, $r, $y;
  $term = 1.0;
  $sum = 1.0;
  $n = 1;
  while ($n <= 10) {
  $denom = floatval(((2 * $n - 1) * (2 * $n)));
  $term = -$term * $x * $x / $denom;
  $sum = $sum + $term;
  $n = $n + 1;
};
  return $sum;
};
  $degreesIncr = 0.1 * $PI / 180.0;
  $turns = 2.0;
  $stop = 360.0 * $turns * 10.0 * $degreesIncr;
  $width = 600.0;
  $centre = $width / 2.0;
  $a = 1.0;
  $b = 20.0;
  $theta = 0.0;
  $count = 0;
  while ($theta < $stop) {
  $r = $a + $b * $theta;
  $x = $r * cosApprox($theta);
  $y = $r * sinApprox($theta);
  if ($count % 100 == 0) {
  echo rtrim(_str($centre + $x) . ',' . _str($centre - $y)), PHP_EOL;
}
  $theta = $theta + $degreesIncr;
  $count = $count + 1;
}
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
