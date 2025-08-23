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
  function totients($limit) {
  $is_prime = [];
  $phi = [];
  $primes = [];
  $i = 0;
  while ($i <= $limit) {
  $is_prime = _append($is_prime, true);
  $phi = _append($phi, $i - 1);
  $i = $i + 1;
};
  $i = 2;
  while ($i <= $limit) {
  if ($is_prime[$i]) {
  $primes = _append($primes, $i);
}
  $j = 0;
  while ($j < count($primes)) {
  $p = $primes[$j];
  if ($i * $p > $limit) {
  break;
}
  $is_prime[$i * $p] = false;
  if ($i % $p == 0) {
  $phi[$i * $p] = $phi[$i] * $p;
  break;
}
  $phi[$i * $p] = $phi[$i] * ($p - 1);
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $phi;
};
  function solution($limit) {
  $phi = totients($limit);
  $total = 0;
  $k = 2;
  while ($k <= $limit) {
  $total = $total + $phi[$k];
  $k = $k + 1;
};
  return $total;
};
  echo rtrim(_str(solution(1000000))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
