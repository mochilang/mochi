<?php
error_reporting(E_ALL & ~E_DEPRECATED);
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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function isqrt($n) {
  $r = 0;
  while (($r + 1) * ($r + 1) <= $n) {
  $r = $r + 1;
};
  return $r;
};
  function prime_sieve($num) {
  if ($num <= 0) {
  _panic('Invalid input, please enter a positive integer.');
}
  $sieve = [];
  $i = 0;
  while ($i <= $num) {
  $sieve = _append($sieve, true);
  $i = $i + 1;
};
  $prime = [];
  $start = 2;
  $end = isqrt($num);
  while ($start <= $end) {
  if ($sieve[$start]) {
  $prime = _append($prime, $start);
  $j = $start * $start;
  while ($j <= $num) {
  if ($sieve[$j]) {
  $sieve[$j] = false;
}
  $j = $j + $start;
};
}
  $start = $start + 1;
};
  $k = $end + 1;
  while ($k <= $num) {
  if ($sieve[$k]) {
  $prime = _append($prime, $k);
}
  $k = $k + 1;
};
  return $prime;
};
  echo rtrim(_str(prime_sieve(50))), PHP_EOL;
  echo rtrim(_str(prime_sieve(25))), PHP_EOL;
  echo rtrim(_str(prime_sieve(10))), PHP_EOL;
  echo rtrim(_str(prime_sieve(9))), PHP_EOL;
  echo rtrim(_str(prime_sieve(2))), PHP_EOL;
  echo rtrim(_str(prime_sieve(1))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
