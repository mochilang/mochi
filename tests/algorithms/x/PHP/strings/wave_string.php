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
$lowercase = 'abcdefghijklmnopqrstuvwxyz';
$uppercase = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
function index_of($s, $c) {
  global $lowercase, $uppercase;
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, _iadd($i, 1) - $i) == $c) {
  return $i;
}
  $i = _iadd($i, 1);
};
  return -1;
}
function is_alpha($c) {
  global $lowercase, $uppercase;
  return index_of($lowercase, $c) >= 0 || index_of($uppercase, $c) >= 0;
}
function to_upper($c) {
  global $lowercase, $uppercase;
  $idx = index_of($lowercase, $c);
  if ($idx >= 0) {
  return substr($uppercase, $idx, _iadd($idx, 1) - $idx);
}
  return $c;
}
function wave($txt) {
  global $lowercase, $uppercase;
  $result = [];
  $i = 0;
  while ($i < strlen($txt)) {
  $ch = substr($txt, $i, _iadd($i, 1) - $i);
  if (is_alpha($ch)) {
  $prefix = substr($txt, 0, $i);
  $suffix = substr($txt, _iadd($i, 1), strlen($txt) - _iadd($i, 1));
  $result = _append($result, $prefix . to_upper($ch) . $suffix);
}
  $i = _iadd($i, 1);
};
  return $result;
}
echo rtrim(_str(wave('cat'))), PHP_EOL;
echo rtrim(_str(wave('one'))), PHP_EOL;
echo rtrim(_str(wave('book'))), PHP_EOL;
