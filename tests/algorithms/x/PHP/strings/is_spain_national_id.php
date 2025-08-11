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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$DIGITS = '0123456789';
$UPPER = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
$LOWER = 'abcdefghijklmnopqrstuvwxyz';
$LOOKUP_LETTERS = 'TRWAGMYFPDXBNJZSQVHLCKE';
$ERROR_MSG = 'Input must be a string of 8 numbers plus letter';
function to_upper($s) {
  global $DIGITS, $UPPER, $LOWER, $LOOKUP_LETTERS, $ERROR_MSG;
  $res = '';
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  $j = 0;
  $converted = $ch;
  while ($j < strlen($LOWER)) {
  if (substr($LOWER, $j, $j + 1 - $j) == $ch) {
  $converted = substr($UPPER, $j, $j + 1 - $j);
  break;
}
  $j = _iadd($j, 1);
};
  $res = $res . $converted;
  $i = _iadd($i, 1);
};
  return $res;
}
function is_digit($ch) {
  global $DIGITS, $UPPER, $LOWER, $LOOKUP_LETTERS, $ERROR_MSG;
  $i = 0;
  while ($i < strlen($DIGITS)) {
  if (substr($DIGITS, $i, $i + 1 - $i) == $ch) {
  return true;
}
  $i = _iadd($i, 1);
};
  return false;
}
function clean_id($spanish_id) {
  global $DIGITS, $UPPER, $LOWER, $LOOKUP_LETTERS, $ERROR_MSG;
  $upper_id = to_upper($spanish_id);
  $cleaned = '';
  $i = 0;
  while ($i < strlen($upper_id)) {
  $ch = substr($upper_id, $i, $i + 1 - $i);
  if ($ch != '-') {
  $cleaned = $cleaned . $ch;
}
  $i = _iadd($i, 1);
};
  return $cleaned;
}
function is_spain_national_id($spanish_id) {
  global $DIGITS, $UPPER, $LOWER, $LOOKUP_LETTERS, $ERROR_MSG;
  $sid = clean_id($spanish_id);
  if (strlen($sid) != 9) {
  _panic($ERROR_MSG);
}
  $i = 0;
  while ($i < 8) {
  if (!is_digit(substr($sid, $i, $i + 1 - $i))) {
  _panic($ERROR_MSG);
}
  $i = _iadd($i, 1);
};
  $number = intval(substr($sid, 0, 8));
  $letter = substr($sid, 8, 8 + 1 - 8);
  if (is_digit($letter)) {
  _panic($ERROR_MSG);
}
  $expected = substr($LOOKUP_LETTERS, _imod($number, 23), _imod($number, 23) + 1 - _imod($number, 23));
  return $letter == $expected;
}
function main() {
  global $DIGITS, $UPPER, $LOWER, $LOOKUP_LETTERS, $ERROR_MSG;
  echo rtrim(json_encode(is_spain_national_id('12345678Z'), 1344)), PHP_EOL;
  echo rtrim(json_encode(is_spain_national_id('12345678z'), 1344)), PHP_EOL;
  echo rtrim(json_encode(is_spain_national_id('12345678x'), 1344)), PHP_EOL;
  echo rtrim(json_encode(is_spain_national_id('12345678I'), 1344)), PHP_EOL;
  echo rtrim(json_encode(is_spain_national_id('12345678-Z'), 1344)), PHP_EOL;
}
main();
