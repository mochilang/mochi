<?php
ini_set('memory_limit', '-1');
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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
function split($s, $sep) {
  $res = [];
  $current = '';
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch == $sep) {
  $res = _append($res, $current);
  $current = '';
} else {
  $current = $current . $ch;
}
  $i = _iadd($i, 1);
};
  $res = _append($res, $current);
  return $res;
}
function join_with_space($xs) {
  $s = '';
  $i = 0;
  while ($i < count($xs)) {
  $s = $s . $xs[$i];
  if (_iadd($i, 1) < count($xs)) {
  $s = $s . ' ';
}
  $i = _iadd($i, 1);
};
  return $s;
}
function reverse_str($s) {
  $res = '';
  $i = _isub(strlen($s), 1);
  while ($i >= 0) {
  $res = $res . substr($s, $i, $i + 1 - $i);
  $i = _isub($i, 1);
};
  return $res;
}
function reverse_letters($sentence, $length) {
  $words = split($sentence, ' ');
  $result = [];
  $i = 0;
  while ($i < count($words)) {
  $word = $words[$i];
  if (strlen($word) > $length) {
  $result = _append($result, reverse_str($word));
} else {
  $result = _append($result, $word);
}
  $i = _iadd($i, 1);
};
  return join_with_space($result);
}
function test_reverse_letters() {
  if (reverse_letters('Hey wollef sroirraw', 3) != 'Hey fellow warriors') {
  _panic('test1 failed');
}
  if (reverse_letters('nohtyP is nohtyP', 2) != 'Python is Python') {
  _panic('test2 failed');
}
  if (reverse_letters('1 12 123 1234 54321 654321', 0) != '1 21 321 4321 12345 123456') {
  _panic('test3 failed');
}
  if (reverse_letters('racecar', 0) != 'racecar') {
  _panic('test4 failed');
}
}
function main() {
  test_reverse_letters();
  echo rtrim(reverse_letters('Hey wollef sroirraw', 3)), PHP_EOL;
}
main();
