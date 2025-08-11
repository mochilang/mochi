<?php
ini_set('memory_limit', '-1');
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
function prime_sieve($limit) {
  global $ans;
  if ($limit <= 2) {
  return [];
}
  $is_prime = [];
  $i = 0;
  while ($i < $limit) {
  $is_prime = _append($is_prime, true);
  $i = _iadd($i, 1);
};
  $is_prime[0] = false;
  $is_prime[1] = false;
  $p = 3;
  while (_imul($p, $p) < $limit) {
  $index = _imul($p, 2);
  while ($index < $limit) {
  $is_prime[$index] = false;
  $index = _iadd($index, $p);
};
  $p = _iadd($p, 2);
};
  $primes = [2];
  $n = 3;
  while ($n < $limit) {
  if ($is_prime[$n]) {
  $primes = _append($primes, $n);
}
  $n = _iadd($n, 2);
};
  return $primes;
}
function solution($ceiling) {
  global $ans;
  $primes = prime_sieve($ceiling);
  $prime_map = [];
  $i = 0;
  while ($i < count($primes)) {
  $prime_map[$primes[$i]] = true;
  $i = _iadd($i, 1);
};
  $prefix = [0];
  $i = 0;
  while ($i < count($primes)) {
  $prefix = _append($prefix, _iadd($prefix[$i], $primes[$i]));
  $i = _iadd($i, 1);
};
  $max_len = 0;
  while ($max_len < count($prefix) && $prefix[$max_len] < $ceiling) {
  $max_len = _iadd($max_len, 1);
};
  $L = $max_len;
  while ($L > 0) {
  $start = 0;
  while (_iadd($start, $L) <= count($primes)) {
  $s = _isub($prefix[_iadd($start, $L)], $prefix[$start]);
  if ($s >= $ceiling) {
  break;
}
  if ($prime_map[$s]) {
  return $s;
}
  $start = _iadd($start, 1);
};
  $L = _isub($L, 1);
};
  return 0;
}
$ans = solution(1000000);
echo rtrim('solution() = ' . _str($ans)), PHP_EOL;
