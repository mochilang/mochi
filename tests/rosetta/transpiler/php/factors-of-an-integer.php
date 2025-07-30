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
  function printFactors($n) {
  if ($n < 1) {
  echo rtrim('
Factors of ' . _str($n) . ' not computed'), PHP_EOL;
  return;
}
  echo rtrim('
Factors of ' . _str($n) . ': '), PHP_EOL;
  $fs = [1];
  $apf = null;
$apf = function($p, $e) use (&$apf, $n, $fs) {
  $orig = count($fs);
  $pp = $p;
  $i = 0;
  while ($i < $e) {
  $j = 0;
  while ($j < $orig) {
  $fs = array_merge($fs, [$fs[$j] * $pp]);
  $j = $j + 1;
};
  $i = $i + 1;
  $pp = $pp * $p;
};
};
  $e = 0;
  $m = $n;
  while ($m % 2 == 0) {
  $m = intval((_intdiv($m, 2)));
  $e = $e + 1;
};
  $apf(2, $e);
  $d = 3;
  while ($m > 1) {
  if ($d * $d > $m) {
  $d = $m;
}
  $e = 0;
  while ($m % $d == 0) {
  $m = intval((_intdiv($m, $d)));
  $e = $e + 1;
};
  if ($e > 0) {
  $apf($d, $e);
}
  $d = $d + 2;
};
  echo rtrim(_str($fs)), PHP_EOL;
  echo rtrim('Number of factors = ' . _str(count($fs))), PHP_EOL;
};
  printFactors(-1);
  printFactors(0);
  printFactors(1);
  printFactors(2);
  printFactors(3);
  printFactors(53);
  printFactors(45);
  printFactors(64);
  printFactors(600851475143);
  printFactors(999999999999999989);
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
