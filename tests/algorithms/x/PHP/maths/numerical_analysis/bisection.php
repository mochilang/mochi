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
} else {
  return $x;
}
};
  function bisection($f, $a, $b) {
  $start = $a;
  $end = $b;
  if ($f($a) == 0.0) {
  return $a;
}
  if ($f($b) == 0.0) {
  return $b;
}
  if ($f($a) * $f($b) > 0.0) {
  $panic('could not find root in given interval.');
}
  $mid = $start + ($end - $start) / 2.0;
  while (abs_float($start - $mid) > 0.0000001) {
  $fmid = $f($mid);
  if ($fmid == 0.0) {
  return $mid;
}
  if ($fmid * $f($start) < 0.0) {
  $end = $mid;
} else {
  $start = $mid;
}
  $mid = $start + ($end - $start) / 2.0;
};
  return $mid;
};
  function f($x) {
  return $x * $x * $x - 2.0 * $x - 5.0;
};
  function main() {
  echo rtrim(_str(bisection('f', 1.0, 1000.0))), PHP_EOL;
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
