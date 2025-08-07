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
  function prime_sieve_eratosthenes($num) {
  if ($num <= 0) {
  $panic('Input must be a positive integer');
}
  $primes = [];
  $i = 0;
  while ($i <= $num) {
  $primes = _append($primes, true);
  $i = $i + 1;
};
  $p = 2;
  while ($p * $p <= $num) {
  if ($primes[$p]) {
  $j = $p * $p;
  while ($j <= $num) {
  $primes[$j] = false;
  $j = $j + $p;
};
}
  $p = $p + 1;
};
  $result = [];
  $k = 2;
  while ($k <= $num) {
  if ($primes[$k]) {
  $result = _append($result, $k);
}
  $k = $k + 1;
};
  return $result;
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
  function test_prime_sieve_eratosthenes() {
  if (!list_eq(prime_sieve_eratosthenes(10), [2, 3, 5, 7])) {
  $panic('test 10 failed');
}
  if (!list_eq(prime_sieve_eratosthenes(20), [2, 3, 5, 7, 11, 13, 17, 19])) {
  $panic('test 20 failed');
}
  if (!list_eq(prime_sieve_eratosthenes(2), [2])) {
  $panic('test 2 failed');
}
  if (count(prime_sieve_eratosthenes(1)) != 0) {
  $panic('test 1 failed');
}
};
  function main() {
  test_prime_sieve_eratosthenes();
  echo rtrim(_str(prime_sieve_eratosthenes(20))), PHP_EOL;
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
