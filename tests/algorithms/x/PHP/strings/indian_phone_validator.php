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
function all_digits($s) {
  if (strlen($s) == 0) {
  return false;
}
  $i = 0;
  while ($i < strlen($s)) {
  $c = substr($s, $i, $i + 1 - $i);
  if ($c < '0' || $c > '9') {
  return false;
}
  $i = _iadd($i, 1);
};
  return true;
}
function indian_phone_validator($phone) {
  $s = $phone;
  if (strlen($s) >= 3 && substr($s, 0, 3) == '+91') {
  $s = substr($s, 3, strlen($s) - 3);
  if (strlen($s) > 0) {
  $c = substr($s, 0, 0 + 1);
  if ($c == '-' || $c == ' ') {
  $s = substr($s, 1, strlen($s) - 1);
};
};
}
  if (strlen($s) > 0 && substr($s, 0, 0 + 1) == '0') {
  $s = substr($s, 1, strlen($s) - 1);
}
  if (strlen($s) >= 2 && substr($s, 0, 2) == '91') {
  $s = substr($s, 2, strlen($s) - 2);
}
  if (strlen($s) != 10) {
  return false;
}
  $first = substr($s, 0, 0 + 1);
  if (!($first == '7' || $first == '8' || $first == '9')) {
  return false;
}
  if (!all_digits($s)) {
  return false;
}
  return true;
}
echo rtrim(_str(indian_phone_validator('+91123456789'))), PHP_EOL;
echo rtrim(_str(indian_phone_validator('+919876543210'))), PHP_EOL;
echo rtrim(_str(indian_phone_validator('01234567896'))), PHP_EOL;
echo rtrim(_str(indian_phone_validator('919876543218'))), PHP_EOL;
echo rtrim(_str(indian_phone_validator('+91-1234567899'))), PHP_EOL;
echo rtrim(_str(indian_phone_validator('+91-9876543218'))), PHP_EOL;
