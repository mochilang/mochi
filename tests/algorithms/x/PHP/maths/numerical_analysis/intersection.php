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
  if ($x < 0.0) {
  return -$x;
}
  return $x;
};
  function intersection($function, $x0, $x1) {
  $x_n = $x0;
  $x_n1 = $x1;
  while (true) {
  if ($x_n == $x_n1 || $function($x_n1) == $function($x_n)) {
  $panic('float division by zero, could not find root');
}
  $numerator = $function($x_n1);
  $denominator = ($function($x_n1) - $function($x_n)) / ($x_n1 - $x_n);
  $x_n2 = $x_n1 - $numerator / $denominator;
  if (abs_float($x_n2 - $x_n1) < 0.00001) {
  return $x_n2;
}
  $x_n = $x_n1;
  $x_n1 = $x_n2;
};
};
  function f($x) {
  return $x * $x * $x - 2.0 * $x - 5.0;
};
  function main() {
  echo rtrim(_str(intersection('f', 3.0, 3.5))), PHP_EOL;
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
