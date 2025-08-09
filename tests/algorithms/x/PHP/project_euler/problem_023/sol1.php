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
function int_sqrt($n) {
  $x = 1;
  while (_imul((_iadd($x, 1)), (_iadd($x, 1))) <= $n) {
  $x = _iadd($x, 1);
};
  return $x;
}
function solution($limit) {
  $sum_divs = [];
  $i = 0;
  while ($i <= $limit) {
  $sum_divs = _append($sum_divs, 1);
  $i = _iadd($i, 1);
};
  $sqrt_limit = int_sqrt($limit);
  $i = 2;
  while ($i <= $sqrt_limit) {
  $sum_divs[_imul($i, $i)] = _iadd($sum_divs[_imul($i, $i)], $i);
  $k = _iadd($i, 1);
  while ($k <= _intdiv($limit, $i)) {
  $sum_divs[_imul($k, $i)] = _iadd(_iadd($sum_divs[_imul($k, $i)], $k), $i);
  $k = _iadd($k, 1);
};
  $i = _iadd($i, 1);
};
  $is_abundant = [];
  $i = 0;
  while ($i <= $limit) {
  $is_abundant = _append($is_abundant, false);
  $i = _iadd($i, 1);
};
  $abundants = [];
  $res = 0;
  $n = 1;
  while ($n <= $limit) {
  if ($sum_divs[$n] > $n) {
  $abundants = _append($abundants, $n);
  $is_abundant[$n] = true;
}
  $has_pair = false;
  $j = 0;
  while ($j < count($abundants)) {
  $a = $abundants[$j];
  if ($a > $n) {
  break;
}
  $b = _isub($n, $a);
  if ($b <= $limit && $is_abundant[$b]) {
  $has_pair = true;
  break;
}
  $j = _iadd($j, 1);
};
  if (!$has_pair) {
  $res = _iadd($res, $n);
}
  $n = _iadd($n, 1);
};
  return $res;
}
echo rtrim(_str(solution(28123))), PHP_EOL;
