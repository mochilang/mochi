<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
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
function palindromic_string($input_string) {
  $max_length = 0;
  $new_input_string = '';
  $output_string = '';
  $n = strlen($input_string);
  $i = 0;
  while ($i < _isub($n, 1)) {
  $new_input_string = $new_input_string . substr($input_string, $i, _iadd($i, 1) - $i) . '|';
  $i = _iadd($i, 1);
};
  $new_input_string = $new_input_string . substr($input_string, _isub($n, 1), $n - _isub($n, 1));
  $left = 0;
  $right = 0;
  $length = [];
  $i = 0;
  $m = strlen($new_input_string);
  while ($i < $m) {
  $length = _append($length, 1);
  $i = _iadd($i, 1);
};
  $start = 0;
  $j = 0;
  while ($j < $m) {
  $k = 1;
  if ($j <= $right) {
  $mirror = _isub(_iadd($left, $right), $j);
  $k = _idiv($length[$mirror], 2);
  $diff = _iadd(_isub($right, $j), 1);
  if ($diff < $k) {
  $k = $diff;
};
  if ($k < 1) {
  $k = 1;
};
}
  while (_isub($j, $k) >= 0 && _iadd($j, $k) < $m && substr($new_input_string, _iadd($j, $k), _iadd(_iadd($j, $k), 1) - _iadd($j, $k)) == substr($new_input_string, _isub($j, $k), _iadd(_isub($j, $k), 1) - _isub($j, $k))) {
  $k = _iadd($k, 1);
};
  $length[$j] = _isub(_imul(2, $k), 1);
  if (_isub(_iadd($j, $k), 1) > $right) {
  $left = _iadd(_isub($j, $k), 1);
  $right = _isub(_iadd($j, $k), 1);
}
  if ($length[$j] > $max_length) {
  $max_length = $length[$j];
  $start = $j;
}
  $j = _iadd($j, 1);
};
  $s = substr($new_input_string, _isub($start, _intdiv($max_length, 2)), _iadd(_iadd($start, _intdiv($max_length, 2)), 1) - _isub($start, _intdiv($max_length, 2)));
  $idx = 0;
  while ($idx < strlen($s)) {
  $ch = substr($s, $idx, _iadd($idx, 1) - $idx);
  if ($ch != '|') {
  $output_string = $output_string . $ch;
}
  $idx = _iadd($idx, 1);
};
  return $output_string;
}
function main() {
  echo rtrim(palindromic_string('abbbaba')), PHP_EOL;
  echo rtrim(palindromic_string('ababa')), PHP_EOL;
}
main();
