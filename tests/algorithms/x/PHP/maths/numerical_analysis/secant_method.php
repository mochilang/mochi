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
  function exp_approx($x) {
  $sum = 1.0;
  $term = 1.0;
  $i = 1;
  while ($i <= 20) {
  $term = $term * $x / $i;
  $sum = $sum + $term;
  $i = $i + 1;
};
  return $sum;
};
  function f($x) {
  return 8.0 * $x - 2.0 * exp_approx(-$x);
};
  function secant_method($lower_bound, $upper_bound, $repeats) {
  $x0 = $lower_bound;
  $x1 = $upper_bound;
  $i = 0;
  while ($i < $repeats) {
  $fx1 = f($x1);
  $fx0 = f($x0);
  $new_x = $x1 - ($fx1 * ($x1 - $x0)) / ($fx1 - $fx0);
  $x0 = $x1;
  $x1 = $new_x;
  $i = $i + 1;
};
  return $x1;
};
  echo rtrim(_str(secant_method(1.0, 3.0, 2))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
