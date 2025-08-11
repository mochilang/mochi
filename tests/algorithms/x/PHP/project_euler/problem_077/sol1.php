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
$NUM_PRIMES = 100;
function generate_primes($limit) {
  global $NUM_PRIMES, $partition_cache, $result;
  $is_prime = [];
  $i = 0;
  while ($i <= $limit) {
  $is_prime = _append($is_prime, true);
  $i = _iadd($i, 1);
};
  $is_prime[0] = false;
  $is_prime[1] = false;
  $i = 2;
  while (_imul($i, $i) <= $limit) {
  if ($is_prime[$i]) {
  $j = _imul($i, $i);
  while ($j <= $limit) {
  $is_prime[$j] = false;
  $j = _iadd($j, $i);
};
}
  $i = _iadd($i, 1);
};
  $primes = [];
  $i = 2;
  while ($i <= $limit) {
  if ($is_prime[$i]) {
  $primes = _append($primes, $i);
}
  $i = _iadd($i, 1);
};
  return $primes;
}
$primes = generate_primes($NUM_PRIMES);
function contains($xs, $value) {
  global $NUM_PRIMES, $primes, $partition_cache, $result;
  $i = 0;
  while ($i < count($xs)) {
  if ($xs[$i] == $value) {
  return true;
}
  $i = _iadd($i, 1);
};
  return false;
}
$partition_cache = [];
function partition($n) {
  global $NUM_PRIMES, $primes, $partition_cache, $result;
  if ($n < 0) {
  return [];
}
  if ($n == 0) {
  return [1];
}
  if (array_key_exists($n, $partition_cache)) {
  return $partition_cache[$n];
}
  $ret = [];
  foreach ($primes as $prime) {
  if ($prime > $n) {
  continue;
}
  $subs = partition(_isub($n, $prime));
  foreach ($subs as $sub) {
  $prod = _imul($sub, $prime);
  if (!contains($ret, $prod)) {
  $ret = _append($ret, $prod);
}
};
};
  $partition_cache[$n] = $ret;
  return $ret;
}
function solution($threshold) {
  global $NUM_PRIMES, $primes, $partition_cache, $result;
  $number_to_partition = 1;
  while ($number_to_partition < $NUM_PRIMES) {
  $parts = partition($number_to_partition);
  if (count($parts) > $threshold) {
  return $number_to_partition;
}
  $number_to_partition = _iadd($number_to_partition, 1);
};
  return 0;
}
$result = solution(5000);
echo rtrim('solution() = ' . _str($result)), PHP_EOL;
