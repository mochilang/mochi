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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
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
  $LIMIT = 10000;
  $sieve = [];
  $i = 0;
  while ($i <= $LIMIT) {
  $sieve = _append($sieve, true);
  $i = _iadd($i, 1);
}
  $p = 2;
  while (_imul($p, $p) <= $LIMIT) {
  if ($sieve[$p]) {
  $j = _imul($p, $p);
  while ($j <= $LIMIT) {
  $sieve[$j] = false;
  $j = _iadd($j, $p);
};
}
  $p = _iadd($p, 1);
}
  function is_prime($n) {
  global $LIMIT, $sieve, $i, $p, $j;
  return $sieve[$n];
};
  function contains_an_even_digit($n) {
  global $LIMIT, $sieve, $i, $p, $j;
  $s = _str($n);
  $idx = 0;
  while ($idx < strlen($s)) {
  $c = substr($s, $idx, $idx + 1 - $idx);
  if ($c == '0' || $c == '2' || $c == '4' || $c == '6' || $c == '8') {
  return true;
}
  $idx = _iadd($idx, 1);
};
  return false;
};
  function parse_int($s) {
  global $LIMIT, $sieve, $i, $p, $j;
  $value = 0;
  $k = 0;
  while ($k < strlen($s)) {
  $ch = substr($s, $k, $k + 1 - $k);
  $value = _iadd(_imul($value, 10), (intval($ch)));
  $k = _iadd($k, 1);
};
  return $value;
};
  function find_circular_primes($limit) {
  global $LIMIT, $sieve, $i, $p;
  $result = [2];
  $num = 3;
  while ($num <= $limit) {
  if (is_prime($num) && (contains_an_even_digit($num) == false)) {
  $s = _str($num);
  $all_prime = true;
  $j = 0;
  while ($j < strlen($s)) {
  $rotated_str = substr($s, $j, strlen($s) - $j) . substr($s, 0, $j);
  $rotated = parse_int($rotated_str);
  if (!is_prime($rotated)) {
  $all_prime = false;
  break;
}
  $j = _iadd($j, 1);
};
  if ($all_prime) {
  $result = _append($result, $num);
};
}
  $num = _iadd($num, 2);
};
  return $result;
};
  function solution() {
  global $LIMIT, $sieve, $i, $p, $j;
  return count(find_circular_primes($LIMIT));
};
  echo rtrim('len(find_circular_primes()) = ' . _str(solution())), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
