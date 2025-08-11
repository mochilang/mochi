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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
function has_alpha($s) {
  global $res, $x;
  $i = 0;
  while ($i < strlen($s)) {
  $c = substr($s, $i, $i + 1 - $i);
  if (($c >= 'a' && $c <= 'z') || ($c >= 'A' && $c <= 'Z')) {
  return true;
}
  $i = _iadd($i, 1);
};
  return false;
}
function parse_decimal($s) {
  global $res, $x;
  $value = 0;
  $i = 0;
  while ($i < strlen($s)) {
  $c = substr($s, $i, $i + 1 - $i);
  if ($c < '0' || $c > '9') {
  _panic('Non-digit character encountered');
}
  $value = _iadd(_imul($value, 10), (intval($c)));
  $i = _iadd($i, 1);
};
  return $value;
}
function get_barcode($barcode) {
  global $res, $x;
  if (has_alpha($barcode)) {
  _panic('Barcode \'' . $barcode . '\' has alphabetic characters.');
}
  if (strlen($barcode) > 0 && substr($barcode, 0, 0 + 1) == '-') {
  _panic('The entered barcode has a negative value. Try again.');
}
  return parse_decimal($barcode);
}
function get_check_digit($barcode) {
  global $res, $x;
  $num = _intdiv($barcode, 10);
  $s = 0;
  $position = 0;
  while ($num != 0) {
  $mult = (_imod($position, 2) == 0 ? 3 : 1);
  $s = _iadd($s, _imul($mult, (_imod($num, 10))));
  $num = _intdiv($num, 10);
  $position = _iadd($position, 1);
};
  return _imod((_isub(10, (_imod($s, 10)))), 10);
}
function is_valid($barcode) {
  global $res, $x;
  return strlen(_str($barcode)) == 13 && get_check_digit($barcode) == _imod($barcode, 10);
}
echo rtrim(_str(get_check_digit(8718452538119))), PHP_EOL;
echo rtrim(_str(get_check_digit(87184523))), PHP_EOL;
echo rtrim(_str(get_check_digit(87193425381086))), PHP_EOL;
$res = [];
$x = 0;
while ($x < 100) {
  $res = _append($res, get_check_digit($x));
  $x = _iadd($x, 10);
}
echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($res, 1344)))))), PHP_EOL;
echo rtrim(_str(is_valid(8718452538119))), PHP_EOL;
echo rtrim(_str(is_valid(87184525))), PHP_EOL;
echo rtrim(_str(is_valid(87193425381089))), PHP_EOL;
echo rtrim(_str(is_valid(0))), PHP_EOL;
echo rtrim(_str(get_barcode('8718452538119'))), PHP_EOL;
