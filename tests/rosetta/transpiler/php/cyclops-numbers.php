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
function parseIntStr($s, $base = 10) {
    return intval($s, intval($base));
}
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
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
  function digits($n) {
  if ($n == 0) {
  return [0];
}
  $rev = [];
  $x = $n;
  while ($x > 0) {
  $rev = _append($rev, $x % 10);
  $x = intval((_intdiv($x, 10)));
};
  $out = [];
  $i = count($rev) - 1;
  while ($i >= 0) {
  $out = _append($out, $rev[$i]);
  $i = $i - 1;
};
  return $out;
};
  function commatize($n) {
  $s = _str($n);
  $out = '';
  $i = strlen($s);
  while ($i > 3) {
  $out = ',' . substr($s, $i - 3, $i - ($i - 3)) . $out;
  $i = $i - 3;
};
  $out = substr($s, 0, $i - 0) . $out;
  return $out;
};
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
  function split($s, $sep) {
  $parts = [];
  $cur = '';
  $i = 0;
  while ($i < strlen($s)) {
  if ($i + strlen($sep) <= strlen($s) && substr($s, $i, $i + strlen($sep) - $i) == $sep) {
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
  function mochi_parseIntStr($str) {
  $i = 0;
  $neg = false;
  if (strlen($str) > 0 && substr($str, 0, 1 - 0) == '-') {
  $neg = true;
  $i = 1;
}
  $n = 0;
  $digits = ['0' => 0, '1' => 1, '2' => 2, '3' => 3, '4' => 4, '5' => 5, '6' => 6, '7' => 7, '8' => 8, '9' => 9];
  while ($i < strlen($str)) {
  $n = $n * 10 . substr('digits', substr($str, $i, $i + 1 - $i), substr($str, $i, $i + 1 - $i) + 1 - substr($str, $i, $i + 1 - $i));
  $i = $i + 1;
};
  if ($neg) {
  $n = -$n;
}
  return $n;
};
  function reverseStr($s) {
  $out = '';
  $i = strlen($s) - 1;
  while ($i >= 0) {
  $out = $out . substr($s, $i, $i + 1 - $i);
  $i = $i - 1;
};
  return $out;
};
  function pad($s, $w) {
  $out = $s;
  while (strlen($out) < $w) {
  $out = ' ' . $out;
};
  return $out;
};
  function findFirst($list) {
  $i = 0;
  while ($i < count($list)) {
  if ($list[$i] > 10000000) {
  return [$list[$i], $i];
}
  $i = $i + 1;
};
  return [-1, -1];
};
  function main() {
  $ranges = [[0, 0], [101, 909], [11011, 99099], [1110111, 9990999], [111101111, 119101111]];
  $cyclops = [];
  foreach ($ranges as $r) {
  $start = $r[0];
  $end = $r[1];
  $numDigits = strlen(_str($start));
  $center = _intdiv($numDigits, 2);
  $i = $start;
  while ($i <= $end) {
  $ds = digits($i);
  if ($ds[$center] == 0) {
  $count = 0;
  foreach ($ds as $d) {
  if ($d == 0) {
  $count = $count + 1;
}
};
  if ($count == 1) {
  $cyclops = _append($cyclops, $i);
};
}
  $i = $i + 1;
};
};
  echo rtrim('The first 50 cyclops numbers are:'), PHP_EOL;
  $idx = 0;
  while ($idx < 50) {
  echo rtrim(pad(commatize($cyclops[$idx]), 6) . ' '), PHP_EOL;
  $idx = $idx + 1;
  if ($idx % 10 == 0) {
  echo rtrim('
'), PHP_EOL;
}
};
  $fi = findFirst($cyclops);
  echo rtrim('
First such number > 10 million is ' . commatize($fi[0]) . ' at zero-based index ' . commatize($fi[1])), PHP_EOL;
  $primes = [];
  foreach ($cyclops as $n) {
  if (isPrime($n)) {
  $primes = _append($primes, $n);
}
};
  echo rtrim('

The first 50 prime cyclops numbers are:'), PHP_EOL;
  $idx = 0;
  while ($idx < 50) {
  echo rtrim(pad(commatize($primes[$idx]), 6) . ' '), PHP_EOL;
  $idx = $idx + 1;
  if ($idx % 10 == 0) {
  echo rtrim('
'), PHP_EOL;
}
};
  $fp = findFirst($primes);
  echo rtrim('
First such number > 10 million is ' . commatize($fp[0]) . ' at zero-based index ' . commatize($fp[1])), PHP_EOL;
  $bpcyclops = [];
  $ppcyclops = [];
  foreach ($primes as $p) {
  $ps = _str($p);
  $splitp = explode('0', $ps);
  $noMiddle = parseIntStr($splitp[0] . $splitp[1], 10);
  if (isPrime($noMiddle)) {
  $bpcyclops = _append($bpcyclops, $p);
}
  if ($ps == reverseStr($ps)) {
  $ppcyclops = _append($ppcyclops, $p);
}
};
  echo rtrim('

The first 50 blind prime cyclops numbers are:'), PHP_EOL;
  $idx = 0;
  while ($idx < 50) {
  echo rtrim(pad(commatize($bpcyclops[$idx]), 6) . ' '), PHP_EOL;
  $idx = $idx + 1;
  if ($idx % 10 == 0) {
  echo rtrim('
'), PHP_EOL;
}
};
  $fb = findFirst($bpcyclops);
  echo rtrim('
First such number > 10 million is ' . commatize($fb[0]) . ' at zero-based index ' . commatize($fb[1])), PHP_EOL;
  echo rtrim('

The first 50 palindromic prime cyclops numbers are:'), PHP_EOL;
  $idx = 0;
  while ($idx < 50) {
  echo rtrim(pad(commatize($ppcyclops[$idx]), 9) . ' '), PHP_EOL;
  $idx = $idx + 1;
  if ($idx % 8 == 0) {
  echo rtrim('
'), PHP_EOL;
}
};
  $fpp = findFirst($ppcyclops);
  echo rtrim('

First such number > 10 million is ' . commatize($fpp[0]) . ' at zero-based index ' . commatize($fpp[1])), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
