<?php
ini_set('memory_limit', '-1');
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
function capitalize($sentence) {
  global $lowercase, $uppercase;
  if (strlen($sentence) == 0) {
  return '';
}
  $first = substr($sentence, 0, 1);
  $idx = index_of($lowercase, $first);
  $capital = ($idx >= 0 ? substr($uppercase, $idx, _iadd($idx, 1) - $idx) : $first);
  return $capital . substr($sentence, 1, strlen($sentence) - 1);
}
echo rtrim(capitalize('hello world')), PHP_EOL;
echo rtrim(capitalize('123 hello world')), PHP_EOL;
echo rtrim(capitalize(' hello world')), PHP_EOL;
echo rtrim(capitalize('a')), PHP_EOL;
echo rtrim(capitalize('')), PHP_EOL;
