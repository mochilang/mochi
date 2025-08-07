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
  $seed = 1;
  function next_seed($x) {
  global $PI, $seed;
  return ($x * 1103515245 + 12345) % 2147483648;
};
  function rand_unit() {
  global $PI, $seed;
  $seed = next_seed($seed);
  return (floatval($seed)) / 2147483648.0;
};
  function is_in_unit_circle($p) {
  global $PI, $seed;
  return $p['x'] * $p['x'] + $p['y'] * $p['y'] <= 1.0;
};
  function random_unit_square() {
  global $PI, $seed;
  return ['x' => rand_unit(), 'y' => rand_unit()];
};
  function estimate_pi($simulations) {
  global $PI, $seed;
  if ($simulations < 1) {
  $panic('At least one simulation is necessary to estimate PI.');
}
  $inside = 0;
  $i = 0;
  while ($i < $simulations) {
  $p = random_unit_square();
  if (is_in_unit_circle($p)) {
  $inside = $inside + 1;
}
  $i = $i + 1;
};
  return 4.0 * (floatval($inside)) / (floatval($simulations));
};
  function abs_float($x) {
  global $PI, $seed;
  if ($x < 0.0) {
  return -$x;
}
  return $x;
};
  function main() {
  global $PI, $seed;
  $n = 10000;
  $my_pi = estimate_pi($n);
  $error = abs_float($my_pi - $PI);
  echo rtrim('An estimate of PI is ' . _str($my_pi) . ' with an error of ' . _str($error)), PHP_EOL;
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
