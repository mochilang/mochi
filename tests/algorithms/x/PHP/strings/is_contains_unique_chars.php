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
function mochi_ord($ch) {
  $lower = 'abcdefghijklmnopqrstuvwxyz';
  $upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $digits = '0123456789';
  $i = 0;
  while ($i < strlen($lower)) {
  if (substr($lower, $i, $i + 1 - $i) == $ch) {
  return _iadd(97, $i);
}
  $i = _iadd($i, 1);
};
  $i = 0;
  while ($i < strlen($upper)) {
  if (substr($upper, $i, $i + 1 - $i) == $ch) {
  return _iadd(65, $i);
}
  $i = _iadd($i, 1);
};
  $i = 0;
  while ($i < strlen($digits)) {
  if (substr($digits, $i, $i + 1 - $i) == $ch) {
  return _iadd(48, $i);
}
  $i = _iadd($i, 1);
};
  if ($ch == ' ') {
  return 32;
}
  if ($ch == '_') {
  return 95;
}
  if ($ch == '.') {
  return 46;
}
  if ($ch == '\'') {
  return 39;
}
  return 0;
}
function lshift($num, $k) {
  $result = $num;
  $i = 0;
  while ($i < $k) {
  $result = _imul($result, 2);
  $i = _iadd($i, 1);
};
  return $result;
}
function rshift($num, $k) {
  $result = $num;
  $i = 0;
  while ($i < $k) {
  $result = _intdiv((_isub($result, (_imod($result, 2)))), 2);
  $i = _iadd($i, 1);
};
  return $result;
}
function is_contains_unique_chars($input_str) {
  $bitmap = 0;
  $i = 0;
  while ($i < strlen($input_str)) {
  $code = mochi_ord(substr($input_str, $i, $i + 1 - $i));
  if (_imod(rshift($bitmap, $code), 2) == 1) {
  return false;
}
  $bitmap = _iadd($bitmap, lshift(1, $code));
  $i = _iadd($i, 1);
};
  return true;
}
echo rtrim(_str(is_contains_unique_chars('I_love.py'))), PHP_EOL;
echo rtrim(_str(is_contains_unique_chars('I don\'t love Python'))), PHP_EOL;
