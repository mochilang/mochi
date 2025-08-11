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
$LOWER = 'abcdefghijklmnopqrstuvwxyz';
$UPPER = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
$DIGITS = '0123456789';
function is_lower($ch) {
  global $LOWER, $UPPER, $DIGITS;
  $i = 0;
  while ($i < strlen($LOWER)) {
  if (substr($LOWER, $i, $i + 1 - $i) == $ch) {
  return true;
}
  $i = _iadd($i, 1);
};
  return false;
}
function is_upper($ch) {
  global $LOWER, $UPPER, $DIGITS;
  $i = 0;
  while ($i < strlen($UPPER)) {
  if (substr($UPPER, $i, $i + 1 - $i) == $ch) {
  return true;
}
  $i = _iadd($i, 1);
};
  return false;
}
function is_digit($ch) {
  global $LOWER, $UPPER, $DIGITS;
  $i = 0;
  while ($i < strlen($DIGITS)) {
  if (substr($DIGITS, $i, $i + 1 - $i) == $ch) {
  return true;
}
  $i = _iadd($i, 1);
};
  return false;
}
function is_alpha($ch) {
  global $LOWER, $UPPER, $DIGITS;
  if (is_lower($ch)) {
  return true;
}
  if (is_upper($ch)) {
  return true;
}
  return false;
}
function is_alnum($ch) {
  global $LOWER, $UPPER, $DIGITS;
  if (is_alpha($ch)) {
  return true;
}
  if (is_digit($ch)) {
  return true;
}
  return false;
}
function to_lower($ch) {
  global $LOWER, $UPPER, $DIGITS;
  $i = 0;
  while ($i < strlen($UPPER)) {
  if (substr($UPPER, $i, $i + 1 - $i) == $ch) {
  return substr($LOWER, $i, $i + 1 - $i);
}
  $i = _iadd($i, 1);
};
  return $ch;
}
function camel_to_snake_case($input_str) {
  global $LOWER, $UPPER, $DIGITS;
  $snake_str = '';
  $i = 0;
  $prev_is_digit = false;
  $prev_is_alpha = false;
  while ($i < strlen($input_str)) {
  $ch = substr($input_str, $i, $i + 1 - $i);
  if (is_upper($ch)) {
  $snake_str = $snake_str . '_' . to_lower($ch);
} else {
  if ($prev_is_digit && is_lower($ch)) {
  $snake_str = $snake_str . '_' . $ch;
} else {
  if ($prev_is_alpha && is_digit($ch)) {
  $snake_str = $snake_str . '_' . $ch;
} else {
  if (!is_alnum($ch)) {
  $snake_str = $snake_str . '_';
} else {
  $snake_str = $snake_str . $ch;
};
};
};
}
  $prev_is_digit = is_digit($ch);
  $prev_is_alpha = is_alpha($ch);
  $i = _iadd($i, 1);
};
  if (strlen($snake_str) > 0 && substr($snake_str, 0, 0 + 1) == '_') {
  $snake_str = substr($snake_str, 1, strlen($snake_str) - 1);
}
  return $snake_str;
}
function main() {
  global $LOWER, $UPPER, $DIGITS;
  echo rtrim(camel_to_snake_case('someRandomString')), PHP_EOL;
  echo rtrim(camel_to_snake_case('SomeRandomStr#ng')), PHP_EOL;
  echo rtrim(camel_to_snake_case('123SomeRandom123String123')), PHP_EOL;
}
main();
