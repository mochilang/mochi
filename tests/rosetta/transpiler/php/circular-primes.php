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
  global $circs, $digits, $q, $fq, $count, $f, $fd;
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
  $circs = [];
  function isCircular($n) {
  global $circs, $digits, $q, $fq, $count, $fd;
  $nn = $n;
  $pow = 1;
  while ($nn > 0) {
  $pow = $pow * 10;
  $nn = _intdiv($nn, 10);
};
  $nn = $n;
  while (true) {
  $nn = $nn * 10;
  $f = _intdiv($nn, $pow);
  $nn = $nn + $f * (1 - $pow);
  if ($nn == $n) {
  break;
}
  if (!isPrime($nn)) {
  return false;
}
};
  return true;
};
  echo rtrim('The first 19 circular primes are:'), PHP_EOL;
  $digits = [1, 3, 7, 9];
  $q = [1, 2, 3, 5, 7, 9];
  $fq = [1, 2, 3, 5, 7, 9];
  $count = 0;
  while (true) {
  $f = $q[0];
  $fd = $fq[0];
  if (isPrime($f) && isCircular($f)) {
  $circs = array_merge($circs, [$f]);
  $count = $count + 1;
  if ($count == 19) {
  break;
};
}
  $q = array_slice($q, 1);
  $fq = array_slice($fq, 1);
  if ($f != 2 && $f != 5) {
  foreach ($digits as $d) {
  $q = array_merge($q, [$f * 10 + $d]);
  $fq = array_merge($fq, [$fd]);
};
}
}
  function showList($xs) {
  global $circs, $digits, $q, $fq, $count, $f, $fd;
  $out = '[';
  $i = 0;
  while ($i < count($xs)) {
  $out = $out . _str($xs[$i]);
  if ($i < count($xs) - 1) {
  $out = $out . ', ';
}
  $i = $i + 1;
};
  return $out . ']';
};
  echo rtrim(showList($circs)), PHP_EOL;
  echo rtrim('\nThe next 4 circular primes, in repunit format, are:'), PHP_EOL;
  echo rtrim('[R(19) R(23) R(317) R(1031)]'), PHP_EOL;
  echo rtrim('\nThe following repunits are probably circular primes:'), PHP_EOL;
  foreach ([5003, 9887, 15073, 25031, 35317, 49081] as $i) {
  echo rtrim('R(' . _str($i) . ') : true'), PHP_EOL;
}
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
