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
$__start_mem = memory_get_usage();
$__start = _now();
  $NUM_PRIMES = 100;
  function generate_primes($limit) {
  global $NUM_PRIMES, $partition_cache, $result;
  $is_prime = [];
  $i = 0;
  while ($i <= $limit) {
  $is_prime = _append($is_prime, true);
  $i = $i + 1;
};
  $is_prime[0] = false;
  $is_prime[1] = false;
  $i = 2;
  while ($i * $i <= $limit) {
  if ($is_prime[$i]) {
  $j = $i * $i;
  while ($j <= $limit) {
  $is_prime[$j] = false;
  $j = $j + $i;
};
}
  $i = $i + 1;
};
  $primes = [];
  $i = 2;
  while ($i <= $limit) {
  if ($is_prime[$i]) {
  $primes = _append($primes, $i);
}
  $i = $i + 1;
};
  return $primes;
};
  $primes = generate_primes($NUM_PRIMES);
  function mochi_contains($xs, $value) {
  global $NUM_PRIMES, $partition_cache, $primes, $result;
  $i = 0;
  while ($i < count($xs)) {
  if ($xs[$i] == $value) {
  return true;
}
  $i = $i + 1;
};
  return false;
};
  $partition_cache = [];
  function partition($n) {
  global $NUM_PRIMES, $partition_cache, $primes, $result;
  if ($n < 0) {
  return [];
}
  if ($n == 0) {
  return [1];
}
  if (array_key_exists($n, $partition_cache)) {
  return $partition_cache[$n];
}
  $ret = [];
  foreach ($primes as $prime) {
  if ($prime > $n) {
  continue;
}
  $subs = partition($n - $prime);
  foreach ($subs as $sub) {
  $prod = $sub * $prime;
  if (!mochi_contains($ret, $prod)) {
  $ret = _append($ret, $prod);
}
};
};
  $partition_cache[$n] = $ret;
  return $ret;
};
  function solution($threshold) {
  global $NUM_PRIMES, $partition_cache, $primes, $result;
  $number_to_partition = 1;
  while ($number_to_partition < $NUM_PRIMES) {
  $parts = partition($number_to_partition);
  if (count($parts) > $threshold) {
  return $number_to_partition;
}
  $number_to_partition = $number_to_partition + 1;
};
  return 0;
};
  $result = solution(5000);
  echo rtrim('solution() = ' . _str($result)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
