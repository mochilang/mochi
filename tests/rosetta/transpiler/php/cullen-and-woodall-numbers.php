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
function _iadd($a, $b) {
    if (function_exists('bcadd')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcadd($sa, $sb, 0);
    }
    return $a + $b;
}
function _isub($a, $b) {
    if (function_exists('bcsub')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcsub($sa, $sb, 0);
    }
    return $a - $b;
}
function _imul($a, $b) {
    if (function_exists('bcmul')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcmul($sa, $sb, 0);
    }
    return $a * $b;
}
function _idiv($a, $b) {
    return _intdiv($a, $b);
}
function _imod($a, $b) {
    if (function_exists('bcmod')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcmod($sa, $sb));
    }
    return $a % $b;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function pow_big($base, $exp) {
  $result = 1;
  $b = $base;
  $e = $exp;
  while ($e > 0) {
  if ($e % 2 == 1) {
  $result = _imul($result, $b);
}
  $b = _imul($b, $b);
  $e = intval((_intdiv($e, 2)));
};
  return $result;
};
  function cullen($n) {
  $two_n = pow_big(2, $n);
  return _iadd((_imul($two_n, ($n))), (1));
};
  function woodall($n) {
  return _isub(cullen($n), (2));
};
  function show_list($xs) {
  $line = '';
  $i = 0;
  while ($i < count($xs)) {
  $line = $line . _str($xs[$i]);
  if ($i < count($xs) - 1) {
  $line = $line . ' ';
}
  $i = $i + 1;
};
  return $line;
};
  function main() {
  $cnums = [];
  $i = 1;
  while ($i <= 20) {
  $cnums = array_merge($cnums, [cullen($i)]);
  $i = $i + 1;
};
  echo rtrim('First 20 Cullen numbers (n * 2^n + 1):'), PHP_EOL;
  echo rtrim(show_list($cnums)), PHP_EOL;
  $wnums = [];
  $i = 1;
  while ($i <= 20) {
  $wnums = array_merge($wnums, [woodall($i)]);
  $i = $i + 1;
};
  echo rtrim('
First 20 Woodall numbers (n * 2^n - 1):'), PHP_EOL;
  echo rtrim(show_list($wnums)), PHP_EOL;
  $cprimes = [1, 141, 4713, 5795, 6611];
  echo rtrim('
First 5 Cullen primes (in terms of n):'), PHP_EOL;
  echo rtrim(show_list($cprimes)), PHP_EOL;
  $wprimes = [2, 3, 6, 30, 75, 81, 115, 123, 249, 362, 384, 462];
  echo rtrim('
First 12 Woodall primes (in terms of n):'), PHP_EOL;
  echo rtrim(show_list($wprimes)), PHP_EOL;
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
