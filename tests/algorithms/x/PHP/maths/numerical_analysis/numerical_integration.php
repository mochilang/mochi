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
  function abs_float($x) {
  global $i, $area;
  if ($x < 0.0) {
  return -$x;
} else {
  return $x;
}
};
  function trapezoidal_area($f, $x_start, $x_end, $steps) {
  $step = ($x_end - $x_start) / (floatval($steps));
  $x1 = $x_start;
  $fx1 = $f($x_start);
  $area = 0.0;
  $i = 0;
  while ($i < $steps) {
  $x2 = $x1 + $step;
  $fx2 = $f($x2);
  $area = $area + abs_float($fx2 + $fx1) * $step / 2.0;
  $x1 = $x2;
  $fx1 = $fx2;
  $i = $i + 1;
};
  return $area;
};
  function f($x) {
  global $i, $area;
  return $x * $x * $x;
};
  echo rtrim('f(x) = x^3'), PHP_EOL;
  echo rtrim('The area between the curve, x = -10, x = 10 and the x axis is:'), PHP_EOL;
  $i = 10;
  while ($i <= 100000) {
  $area = trapezoidal_area('f', -5.0, 5.0, $i);
  echo rtrim('with ' . _str($i) . ' steps: ' . _str($area)), PHP_EOL;
  $i = $i * 10;
}
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
