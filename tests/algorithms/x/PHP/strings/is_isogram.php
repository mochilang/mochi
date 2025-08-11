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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
function index_of($s, $ch) {
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = _iadd($i, 1);
};
  return -1;
}
function mochi_ord($ch) {
  $upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $lower = 'abcdefghijklmnopqrstuvwxyz';
  $idx = index_of($upper, $ch);
  if ($idx >= 0) {
  return _iadd(65, $idx);
}
  $idx = index_of($lower, $ch);
  if ($idx >= 0) {
  return _iadd(97, $idx);
}
  return -1;
}
function mochi_chr($n) {
  $upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $lower = 'abcdefghijklmnopqrstuvwxyz';
  if ($n >= 65 && $n < 91) {
  return substr($upper, _isub($n, 65), _isub($n, 64) - _isub($n, 65));
}
  if ($n >= 97 && $n < 123) {
  return substr($lower, _isub($n, 97), _isub($n, 96) - _isub($n, 97));
}
  return '?';
}
function to_lower_char($c) {
  $code = mochi_ord($c);
  if ($code >= 65 && $code <= 90) {
  return mochi_chr(_iadd($code, 32));
}
  return $c;
}
function is_alpha($c) {
  $code = mochi_ord($c);
  return ($code >= 65 && $code <= 90) || ($code >= 97 && $code <= 122);
}
function is_isogram($s) {
  $seen = '';
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if (!is_alpha($ch)) {
  _panic('String must only contain alphabetic characters.');
}
  $lower = to_lower_char($ch);
  if (index_of($seen, $lower) >= 0) {
  return false;
}
  $seen = $seen . $lower;
  $i = _iadd($i, 1);
};
  return true;
}
echo rtrim(_str(is_isogram('Uncopyrightable'))), PHP_EOL;
echo rtrim(_str(is_isogram('allowance'))), PHP_EOL;
