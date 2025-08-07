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
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function prime_factors($n) {
  if ($n < 2) {
  return [];
}
  $num = $n;
  $i = 2;
  $factors = [];
  while ($i * $i <= $num) {
  if ($num % $i == 0) {
  $factors = _append($factors, $i);
  $num = _intdiv($num, $i);
} else {
  $i = $i + 1;
}
};
  if ($num > 1) {
  $factors = _append($factors, $num);
}
  return $factors;
};
  function list_eq($a, $b) {
  if (count($a) != count($b)) {
  return false;
}
  $i = 0;
  while ($i < count($a)) {
  if ($a[$i] != $b[$i]) {
  return false;
}
  $i = $i + 1;
};
  return true;
};
  function test_prime_factors() {
  if (!list_eq(prime_factors(0), [])) {
  $panic('prime_factors(0) failed');
}
  if (!list_eq(prime_factors(100), [2, 2, 5, 5])) {
  $panic('prime_factors(100) failed');
}
  if (!list_eq(prime_factors(2560), [2, 2, 2, 2, 2, 2, 2, 2, 2, 5])) {
  $panic('prime_factors(2560) failed');
}
  if (!list_eq(prime_factors(97), [97])) {
  $panic('prime_factors(97) failed');
}
};
  function main() {
  test_prime_factors();
  echo rtrim(_str(prime_factors(100))), PHP_EOL;
  echo rtrim(_str(prime_factors(2560))), PHP_EOL;
  echo rtrim(_str(prime_factors(97))), PHP_EOL;
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
