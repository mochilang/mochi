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
function upper($word) {
  $lower_chars = 'abcdefghijklmnopqrstuvwxyz';
  $upper_chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $result = '';
  $i = 0;
  while ($i < strlen($word)) {
  $c = substr($word, $i, $i + 1 - $i);
  $idx = index_of($lower_chars, $c);
  if ($idx >= 0) {
  $result = $result . substr($upper_chars, $idx, _iadd($idx, 1) - $idx);
} else {
  $result = $result . $c;
}
  $i = _iadd($i, 1);
};
  return $result;
}
echo rtrim(strtoupper('wow')), PHP_EOL;
echo rtrim(strtoupper('Hello')), PHP_EOL;
echo rtrim(strtoupper('WHAT')), PHP_EOL;
echo rtrim(strtoupper('wh[]32')), PHP_EOL;
