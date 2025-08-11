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
function parse_int($s) {
  $value = 0;
  $i = 0;
  while ($i < strlen($s)) {
  $c = substr($s, $i, $i + 1 - $i);
  $value = _iadd(_imul($value, 10), (intval($c)));
  $i = _iadd($i, 1);
};
  return $value;
}
function is_polish_national_id($id) {
  if (strlen($id) == 0) {
  return false;
}
  if (substr($id, 0, 1) == '-') {
  return false;
}
  $input_int = parse_int($id);
  if ($input_int < 10100000 || $input_int > 99923199999) {
  return false;
}
  $month = parse_int(substr($id, 2, 4 - 2));
  if (!(($month >= 1 && $month <= 12) || ($month >= 21 && $month <= 32) || ($month >= 41 && $month <= 52) || ($month >= 61 && $month <= 72) || ($month >= 81 && $month <= 92))) {
  return false;
}
  $day = parse_int(substr($id, 4, 6 - 4));
  if ($day < 1 || $day > 31) {
  return false;
}
  $multipliers = [1, 3, 7, 9, 1, 3, 7, 9, 1, 3];
  $subtotal = 0;
  $i = 0;
  while ($i < count($multipliers)) {
  $digit = parse_int(substr($id, $i, _iadd($i, 1) - $i));
  $subtotal = _iadd($subtotal, _imod((_imul($digit, $multipliers[$i])), 10));
  $i = _iadd($i, 1);
};
  $checksum = _isub(10, (_imod($subtotal, 10)));
  return $checksum == _imod($input_int, 10);
}
echo rtrim(_str(is_polish_national_id('02070803628'))), PHP_EOL;
echo rtrim(_str(is_polish_national_id('02150803629'))), PHP_EOL;
echo rtrim(_str(is_polish_national_id('02075503622'))), PHP_EOL;
echo rtrim(_str(is_polish_national_id('-99012212349'))), PHP_EOL;
echo rtrim(_str(is_polish_national_id('990122123499999'))), PHP_EOL;
echo rtrim(_str(is_polish_national_id('02070803621'))), PHP_EOL;
