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
  function isPrime($n) {
  global $digits, $primes, $i, $line;
  if ($n < 2) {
  return false;
}
  if ($n % 2 == 0) {
  return $n == 2;
}
  $d = 3;
  while ($d * $d <= $n) {
  if ($n % $d == 0) {
  return false;
}
  $d = $d + 2;
};
  return true;
};
  $digits = [9, 8, 7, 6, 5, 4, 3, 2, 1];
  function gen($idx, $cur, $used) {
  global $digits, $primes, $i, $line;
  if ($idx == count($digits)) {
  if ($used && isPrime($cur)) {
  return [$cur];
};
  return [];
}
  $with = gen($idx + 1, $cur * 10 + $digits[$idx], true);
  $without = gen($idx + 1, $cur, $used);
  return array_merge($with, $without);
};
  $primes = gen(0, 0, false);
  function pad($n, $width) {
  global $digits, $primes, $i, $line;
  $s = _str($n);
  while (strlen($s) < $width) {
  $s = ' ' . $s;
};
  return $s;
};
  echo rtrim('There are ' . _str(count($primes)) . ' descending primes, namely:'), PHP_EOL;
  $i = 0;
  $line = '';
  while ($i < count($primes)) {
  $line = $line . pad($primes[$i], 8) . ' ';
  if (($i + 1) % 10 == 0) {
  echo rtrim(substr($line, 0, strlen($line) - 1 - 0)), PHP_EOL;
  $line = '';
}
  $i = $i + 1;
}
  if (strlen($line) > 0) {
  echo rtrim(substr($line, 0, strlen($line) - 1 - 0)), PHP_EOL;
}
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
