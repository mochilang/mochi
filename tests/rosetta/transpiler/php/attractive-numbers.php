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
  function countPrimeFactors($n) {
  if ($n == 1) {
  return 0;
}
  if (isPrime($n)) {
  return 1;
}
  $count = 0;
  $f = 2;
  while (true) {
  if ($n % $f == 0) {
  $count = $count + 1;
  $n = _intdiv($n, $f);
  if ($n == 1) {
  return $count;
};
  if (isPrime($n)) {
  $f = $n;
};
} else {
  if ($f >= 3) {
  $f = $f + 2;
} else {
  $f = 3;
};
}
};
  return $count;
};
  function pad4($n) {
  $s = _str($n);
  while (strlen($s) < 4) {
  $s = ' ' . $s;
};
  return $s;
};
  function main() {
  $max = 120;
  echo rtrim('The attractive numbers up to and including ' . _str($max) . ' are:'), PHP_EOL;
  $count = 0;
  $line = '';
  $lineCount = 0;
  $i = 1;
  while ($i <= $max) {
  $c = countPrimeFactors($i);
  if (isPrime($c)) {
  $line = $line . pad4($i);
  $count = $count + 1;
  $lineCount = $lineCount + 1;
  if ($lineCount == 20) {
  echo rtrim($line), PHP_EOL;
  $line = '';
  $lineCount = 0;
};
}
  $i = $i + 1;
};
  if ($lineCount > 0) {
  echo rtrim($line), PHP_EOL;
}
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
