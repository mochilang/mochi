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
function is_prime($number) {
  if (1 < $number && $number < 4) {
  return true;
} else {
  if ($number < 2 || _imod($number, 2) == 0 || _imod($number, 3) == 0) {
  return false;
};
}
  $i = 5;
  while (_imul($i, $i) <= $number) {
  if (_imod($number, $i) == 0 || _imod($number, (_iadd($i, 2))) == 0) {
  return false;
}
  $i = _iadd($i, 6);
};
  return true;
}
function solution($a_limit, $b_limit) {
  $longest_len = 0;
  $longest_a = 0;
  $longest_b = 0;
  $a = _iadd((_imul(-1, $a_limit)), 1);
  while ($a < $a_limit) {
  $b = 2;
  while ($b < $b_limit) {
  if (is_prime($b)) {
  $count = 0;
  $n = 0;
  while (is_prime(_iadd(_iadd(_imul($n, $n), _imul($a, $n)), $b))) {
  $count = _iadd($count, 1);
  $n = _iadd($n, 1);
};
  if ($count > $longest_len) {
  $longest_len = $count;
  $longest_a = $a;
  $longest_b = $b;
};
}
  $b = _iadd($b, 1);
};
  $a = _iadd($a, 1);
};
  return _imul($longest_a, $longest_b);
}
echo rtrim(_str(solution(1000, 1000))), PHP_EOL;
