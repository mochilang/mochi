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
  function totient($n) {
  $is_prime = [];
  $totients = [];
  $primes = [];
  $i = 0;
  while ($i <= $n) {
  $is_prime = _append($is_prime, true);
  $totients = _append($totients, $i - 1);
  $i = $i + 1;
};
  $i = 2;
  while ($i <= $n) {
  if ($is_prime[$i]) {
  $primes = _append($primes, $i);
}
  $j = 0;
  while ($j < count($primes)) {
  $p = $primes[$j];
  if ($i * $p >= $n) {
  break;
}
  $is_prime[$i * $p] = false;
  if ($i % $p == 0) {
  $totients[$i * $p] = $totients[$i] * $p;
  break;
}
  $totients[$i * $p] = $totients[$i] * ($p - 1);
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $totients;
};
  function test_totient() {
  $expected = [-1, 0, 1, 2, 2, 4, 2, 6, 4, 6, 9];
  $res = totient(10);
  $idx = 0;
  while ($idx < count($expected)) {
  if ($res[$idx] != $expected[$idx]) {
  $panic('totient mismatch at ' . _str($idx));
}
  $idx = $idx + 1;
};
};
  function main() {
  test_totient();
  $n = 10;
  $res = totient($n);
  $i = 1;
  while ($i < $n) {
  echo rtrim(_str($i) . ' has ' . _str($res[$i]) . ' relative primes.'), PHP_EOL;
  $i = $i + 1;
};
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
