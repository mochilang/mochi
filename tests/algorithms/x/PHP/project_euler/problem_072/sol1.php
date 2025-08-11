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
function totients($limit) {
  $is_prime = [];
  $phi = [];
  $primes = [];
  $i = 0;
  while ($i <= $limit) {
  $is_prime = _append($is_prime, true);
  $phi = _append($phi, _isub($i, 1));
  $i = _iadd($i, 1);
};
  $i = 2;
  while ($i <= $limit) {
  if ($is_prime[$i]) {
  $primes = _append($primes, $i);
}
  $j = 0;
  while ($j < count($primes)) {
  $p = $primes[$j];
  if (_imul($i, $p) > $limit) {
  break;
}
  $is_prime[_imul($i, $p)] = false;
  if (_imod($i, $p) == 0) {
  $phi[_imul($i, $p)] = _imul($phi[$i], $p);
  break;
}
  $phi[_imul($i, $p)] = _imul($phi[$i], (_isub($p, 1)));
  $j = _iadd($j, 1);
};
  $i = _iadd($i, 1);
};
  return $phi;
}
function solution($limit) {
  $phi = totients($limit);
  $total = 0;
  $k = 2;
  while ($k <= $limit) {
  $total = _iadd($total, $phi[$k]);
  $k = _iadd($k, 1);
};
  return $total;
}
echo rtrim(_str(solution(1000000))), PHP_EOL;
