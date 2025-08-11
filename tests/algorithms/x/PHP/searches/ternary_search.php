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
function _intdiv($a, $b) {
    if ($b === 0 || $b === '0') {
        throw new DivisionByZeroError();
    }
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
$precision = 10;
function lin_search($left, $right, $array, $target) {
  global $precision;
  $i = $left;
  while ($i < $right) {
  if ($array[$i] == $target) {
  return $i;
}
  $i = _iadd($i, 1);
};
  return -1;
}
function ite_ternary_search($array, $target) {
  global $precision;
  $left = 0;
  $right = _isub(count($array), 1);
  while ($left <= $right) {
  if (_isub($right, $left) < $precision) {
  $idx = lin_search($left, _iadd($right, 1), $array, $target);
  return $idx;
}
  $one_third = _iadd($left, _intdiv((_isub($right, $left)), 3));
  $two_third = _isub($right, _intdiv((_isub($right, $left)), 3));
  if ($array[$one_third] == $target) {
  return $one_third;
}
  if ($array[$two_third] == $target) {
  return $two_third;
}
  if ($target < $array[$one_third]) {
  $right = _isub($one_third, 1);
} else {
  if ($array[$two_third] < $target) {
  $left = _iadd($two_third, 1);
} else {
  $left = _iadd($one_third, 1);
  $right = _isub($two_third, 1);
};
}
};
  return -1;
}
function rec_ternary_search($left, $right, $array, $target) {
  global $precision;
  if ($left <= $right) {
  if (_isub($right, $left) < $precision) {
  $idx = lin_search($left, _iadd($right, 1), $array, $target);
  return $idx;
};
  $one_third = _iadd($left, _intdiv((_isub($right, $left)), 3));
  $two_third = _isub($right, _intdiv((_isub($right, $left)), 3));
  if ($array[$one_third] == $target) {
  return $one_third;
};
  if ($array[$two_third] == $target) {
  return $two_third;
};
  if ($target < $array[$one_third]) {
  return rec_ternary_search($left, _isub($one_third, 1), $array, $target);
};
  if ($array[$two_third] < $target) {
  return rec_ternary_search(_iadd($two_third, 1), $right, $array, $target);
};
  return rec_ternary_search(_iadd($one_third, 1), _isub($two_third, 1), $array, $target);
}
  return -1;
}
function main() {
  global $precision;
  $test_list = [0, 1, 2, 8, 13, 17, 19, 32, 42];
  echo rtrim(_str(ite_ternary_search($test_list, 3))), PHP_EOL;
  echo rtrim(_str(ite_ternary_search($test_list, 13))), PHP_EOL;
  echo rtrim(_str(rec_ternary_search(0, _isub(count($test_list), 1), $test_list, 3))), PHP_EOL;
  echo rtrim(_str(rec_ternary_search(0, _isub(count($test_list), 1), $test_list, 13))), PHP_EOL;
}
main();
