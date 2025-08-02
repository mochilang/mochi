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
  function sieve($limit) {
  $primes = [];
  $i = 0;
  while ($i < $limit) {
  $primes = array_merge($primes, [true]);
  $i = $i + 1;
};
  $primes[0] = false;
  $primes[1] = false;
  $p = 2;
  while ($p * $p < $limit) {
  if ($primes[$p]) {
  $k = $p * $p;
  while ($k < $limit) {
  $primes[$k] = false;
  $k = $k + $p;
};
}
  $p = $p + 1;
};
  return $primes;
};
  function goldbachCount($primes, $n) {
  $c = 0;
  $i = 1;
  while ($i <= _intdiv($n, 2)) {
  if ($primes[$i] && $primes[$n - $i]) {
  $c = $c + 1;
}
  $i = $i + 1;
};
  return $c;
};
  function pad($n) {
  if ($n < 10) {
  return '  ' . _str($n);
}
  if ($n < 100) {
  return ' ' . _str($n);
}
  return _str($n);
};
  function main() {
  $primes = sieve(1000);
  echo rtrim('The first 100 Goldbach numbers:'), PHP_EOL;
  $line = '';
  $n = 2;
  $count = 0;
  while ($count < 100) {
  $v = goldbachCount($primes, 2 * $n);
  $line = $line . pad($v) . ' ';
  $count = $count + 1;
  $n = $n + 1;
  if ($count % 10 == 0) {
  echo rtrim(substr($line, 0, strlen($line) - 1 - 0)), PHP_EOL;
  $line = '';
}
};
  $val = goldbachCount($primes, 1000);
  echo rtrim('
The 1,000th Goldbach number = ' . _str($val)), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
