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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function runge_kutta($f, $y0, $x0, $h, $x_end) {
  $span = ($x_end - $x0) / $h;
  $n = intval($span);
  if (floatval($n) < $span) {
  $n = $n + 1;
}
  $y = [];
  $i = 0;
  while ($i < $n + 1) {
  $y = _append($y, 0.0);
  $i = $i + 1;
};
  $y[0] = $y0;
  $x = $x0;
  $k = 0;
  while ($k < $n) {
  $k1 = $f($x, $y[$k]);
  $k2 = $f($x + 0.5 * $h, $y[$k] + 0.5 * $h * $k1);
  $k3 = $f($x + 0.5 * $h, $y[$k] + 0.5 * $h * $k2);
  $k4 = $f($x + $h, $y[$k] + $h * $k3);
  $y[$k + 1] = $y[$k] + (1.0 / 6.0) * $h * ($k1 + 2.0 * $k2 + 2.0 * $k3 + $k4);
  $x = $x + $h;
  $k = $k + 1;
};
  return $y;
};
  function test_runge_kutta() {
  $f = null;
$f = function($x, $y) use (&$f) {
  return $y;
};
  $result = runge_kutta($f, 1.0, 0.0, 0.01, 5.0);
  $last = $result[count($result) - 1];
  $expected = 148.41315904125113;
  $diff = $last - $expected;
  if ($diff < 0.0) {
  $diff = -$diff;
}
  if ($diff > 0.000001) {
  $panic('runge_kutta failed');
}
};
  function main() {
  test_runge_kutta();
  $f = null;
$f = function($x, $y) use (&$f) {
  return $y;
};
  $r = runge_kutta($f, 1.0, 0.0, 0.1, 1.0);
  echo rtrim(_str($r[count($r) - 1])), PHP_EOL;
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
