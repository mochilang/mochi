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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function mochi_split($s, $sep) {
  global $case_idx, $j, $line, $m, $n, $primes, $rem, $segment, $size, $start, $t;
  $parts = [];
  $cur = '';
  $i = 0;
  while ($i < strlen($s)) {
  if (strlen($sep) > 0 && $i + strlen($sep) <= strlen($s) && substr($s, $i, $i + strlen($sep) - $i) == $sep) {
  $parts = _append($parts, $cur);
  $cur = '';
  $i = $i + strlen($sep);
} else {
  $cur = $cur . substr($s, $i, $i + 1 - $i);
  $i = $i + 1;
}
};
  $parts = _append($parts, $cur);
  return $parts;
};
  function precompute($limit) {
  global $case_idx, $i, $line, $m, $n, $parts, $rem, $segment, $size, $start, $t;
  $sieve = [];
  for ($i = 0; $i < ($limit + 1); $i++) {
  $sieve = _append($sieve, true);
};
  $sieve[0] = false;
  $sieve[1] = false;
  $p = 2;
  while ($p * $p <= $limit) {
  if ($sieve[$p]) {
  $j = $p * $p;
  while ($j <= $limit) {
  $sieve[$j] = false;
  $j = $j + $p;
};
}
  $p = $p + 1;
};
  $primes = [];
  for ($i = 2; $i < ($limit + 1); $i++) {
  if ($sieve[$i]) {
  $primes = _append($primes, $i);
}
};
  return $primes;
};
  $primes = precompute(32000);
  $t = intval(trim(fgets(STDIN)));
  $case_idx = 0;
  while ($case_idx < $t) {
  $line = trim(fgets(STDIN));
  $parts = mochi_split($line, ' ');
  $m = intval($parts[0]);
  $n = intval($parts[1]);
  $size = $n - $m + 1;
  $segment = [];
  for ($i = 0; $i < $size; $i++) {
  $segment = _append($segment, true);
};
  foreach ($primes as $p) {
  if ($p * $p > $n) {
  break;
}
  $start = $p * $p;
  if ($start < $m) {
  $rem = $m % $p;
  if ($rem == 0) {
  $start = $m;
} else {
  $start = $m + ($p - $rem);
};
}
  $j = $start;
  while ($j <= $n) {
  $segment[$j - $m] = false;
  $j = $j + $p;
};
};
  if ($m == 1) {
  $segment[0] = false;
}
  $i = 0;
  while ($i < $size) {
  if ($segment[$i]) {
  echo rtrim(json_encode($i + $m, 1344)), PHP_EOL;
}
  $i = $i + 1;
};
  if ($case_idx < $t - 1) {
  echo rtrim(''), PHP_EOL;
}
  $case_idx = $case_idx + 1;
}
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
