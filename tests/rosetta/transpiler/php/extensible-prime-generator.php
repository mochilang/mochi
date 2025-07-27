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
  function nextPrime($primes, $start) {
  $n = $start;
  while (true) {
  $isP = true;
  $i = 0;
  while ($i < count($primes)) {
  $p = $primes[$i];
  if ($p * $p > $n) {
  break;
}
  if ($n % $p == 0) {
  $isP = false;
  break;
}
  $i = $i + 1;
};
  if ($isP) {
  return $n;
}
  $n = $n + 2;
};
};
  function main() {
  $primes = [2];
  $cand = 3;
  while (count($primes) < 10000) {
  $cand = nextPrime($primes, $cand);
  $primes = array_merge($primes, [$cand]);
  $cand = $cand + 2;
};
  $line = 'First twenty:';
  $i = 0;
  while ($i < 20) {
  $line = $line . ' ' . _str($primes[$i]);
  $i = $i + 1;
};
  echo rtrim($line), PHP_EOL;
  $idx = 0;
  while ($primes[$idx] <= 100) {
  $idx = $idx + 1;
};
  $line = 'Between 100 and 150: ' . _str($primes[$idx]);
  $idx = $idx + 1;
  while ($primes[$idx] < 150) {
  $line = $line . ' ' . _str($primes[$idx]);
  $idx = $idx + 1;
};
  echo rtrim($line), PHP_EOL;
  while ($primes[$idx] <= 7700) {
  $idx = $idx + 1;
};
  $count = 0;
  while ($primes[$idx] < 8000) {
  $count = $count + 1;
  $idx = $idx + 1;
};
  echo rtrim('Number beween 7,700 and 8,000: ' . _str($count)), PHP_EOL;
  echo rtrim('10,000th prime: ' . _str($primes[9999])), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
