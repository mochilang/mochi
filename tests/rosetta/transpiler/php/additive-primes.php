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
        $sa = is_int($a) ? strval($a) : sprintf('%.0f', $a);
        $sb = is_int($b) ? strval($b) : sprintf('%.0f', $b);
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function isPrime($n) {
  if ($n < 2) {
  return false;
}
  if ($n % 2 == 0) {
  return $n == 2;
}
  if ($n % 3 == 0) {
  return $n == 3;
}
  $d = 5;
  while ($d * $d <= $n) {
  if ($n % $d == 0) {
  return false;
}
  $d = $d + 2;
  if ($n % $d == 0) {
  return false;
}
  $d = $d + 4;
};
  return true;
};
  function sumDigits($n) {
  $s = 0;
  $x = $n;
  while ($x > 0) {
  $s = $s + $x % 10;
  $x = intval((_intdiv($x, 10)));
};
  return $s;
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
  echo rtrim('Additive primes less than 500:'), PHP_EOL;
  $count = 0;
  $line = '';
  $lineCount = 0;
  $i = 2;
  while ($i < 500) {
  if (isPrime($i) && isPrime(sumDigits($i))) {
  $count = $count + 1;
  $line = $line . pad($i) . '  ';
  $lineCount = $lineCount + 1;
  if ($lineCount == 10) {
  echo rtrim(json_encode(substr($line, 0, strlen($line) - 2 - 0), 1344)), PHP_EOL;
  $line = '';
  $lineCount = 0;
};
}
  if ($i > 2) {
  $i = $i + 2;
} else {
  $i = $i + 1;
}
};
  if ($lineCount > 0) {
  echo rtrim(json_encode(substr($line, 0, strlen($line) - 2 - 0), 1344)), PHP_EOL;
}
  echo rtrim(_str($count) . ' additive primes found.'), PHP_EOL;
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
