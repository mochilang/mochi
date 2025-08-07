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
  function gcd($a, $b) {
  $x = $a;
  $y = $b;
  while ($y != 0) {
  $t = $x % $y;
  $x = $y;
  $y = $t;
};
  if ($x < 0) {
  return -$x;
}
  return $x;
};
  function proper_fractions($den) {
  if ($den < 0) {
  $panic('The Denominator Cannot be less than 0');
}
  $res = [];
  $n = 1;
  while ($n < $den) {
  if (gcd($n, $den) == 1) {
  $res = _append($res, _str($n) . '/' . _str($den));
}
  $n = $n + 1;
};
  return $res;
};
  function test_proper_fractions() {
  $a = proper_fractions(10);
  if ($a != ['1/10', '3/10', '7/10', '9/10']) {
  $panic('test 10 failed');
}
  $b = proper_fractions(5);
  if ($b != ['1/5', '2/5', '3/5', '4/5']) {
  $panic('test 5 failed');
}
  $c = proper_fractions(0);
  if ($c != []) {
  $panic('test 0 failed');
}
};
  function main() {
  test_proper_fractions();
  echo rtrim(_str(proper_fractions(10))), PHP_EOL;
  echo rtrim(_str(proper_fractions(5))), PHP_EOL;
  echo rtrim(_str(proper_fractions(0))), PHP_EOL;
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
