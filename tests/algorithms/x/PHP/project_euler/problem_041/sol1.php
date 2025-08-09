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
function is_prime($number) {
  if ((1 < $number) && ($number < 4)) {
  return true;
} else {
  if (($number < 2) || (_imod($number, 2) == 0) || (_imod($number, 3) == 0)) {
  return false;
};
}
  $i = 5;
  while (_imul($i, $i) <= $number) {
  if ((_imod($number, $i) == 0) || (_imod($number, (_iadd($i, 2))) == 0)) {
  return false;
}
  $i = _iadd($i, 6);
};
  return true;
}
function remove_at($xs, $index) {
  $res = [];
  $i = 0;
  while ($i < count($xs)) {
  if ($i != $index) {
  $res = _append($res, $xs[$i]);
}
  $i = _iadd($i, 1);
};
  return $res;
}
function collect_primes($nums, $current, $primes) {
  if (count($nums) == 0) {
  if (is_prime($current)) {
  $primes = _append($primes, $current);
};
  return $primes;
}
  $i = 0;
  $res = $primes;
  while ($i < count($nums)) {
  $digit = $nums[$i];
  $remaining = remove_at($nums, $i);
  $res = collect_primes($remaining, _iadd(_imul($current, 10), $digit), $res);
  $i = _iadd($i, 1);
};
  return $res;
}
function max_list($nums) {
  $m = 0;
  $i = 0;
  while ($i < count($nums)) {
  if ($nums[$i] > $m) {
  $m = $nums[$i];
}
  $i = _iadd($i, 1);
};
  return $m;
}
function solution($n) {
  $digits = [];
  $i = 1;
  while ($i <= $n) {
  $digits = _append($digits, $i);
  $i = _iadd($i, 1);
};
  $primes = collect_primes($digits, 0, []);
  if (count($primes) == 0) {
  return 0;
}
  return max_list($primes);
}
echo rtrim('solution() = ' . _str(solution(7))), PHP_EOL;
