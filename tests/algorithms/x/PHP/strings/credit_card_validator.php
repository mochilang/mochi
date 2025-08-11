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
function validate_initial_digits($cc) {
  return substr($cc, 0, 2) == '34' || substr($cc, 0, 2) == '35' || substr($cc, 0, 2) == '37' || substr($cc, 0, 1) == '4' || substr($cc, 0, 1) == '5' || substr($cc, 0, 1) == '6';
}
function luhn_validation($cc) {
  $sum = 0;
  $double_digit = false;
  $i = _isub(strlen($cc), 1);
  while ($i >= 0) {
  $n = (ctype_digit($cc[$i]) ? intval($cc[$i]) : ord($cc[$i]));
  if ($double_digit) {
  $n = _imul($n, 2);
  if ($n > 9) {
  $n = _isub($n, 9);
};
}
  $sum = _iadd($sum, $n);
  $double_digit = !$double_digit;
  $i = _isub($i, 1);
};
  return _imod($sum, 10) == 0;
}
function is_digit_string($s) {
  $i = 0;
  while ($i < strlen($s)) {
  $c = substr($s, $i, _iadd($i, 1) - $i);
  if ($c < '0' || $c > '9') {
  return false;
}
  $i = _iadd($i, 1);
};
  return true;
}
function validate_credit_card_number($cc) {
  $error_message = $cc . ' is an invalid credit card number because';
  if (!is_digit_string($cc)) {
  echo rtrim($error_message . ' it has nonnumerical characters.'), PHP_EOL;
  return false;
}
  if (!(strlen($cc) >= 13 && strlen($cc) <= 16)) {
  echo rtrim($error_message . ' of its length.'), PHP_EOL;
  return false;
}
  if (!validate_initial_digits($cc)) {
  echo rtrim($error_message . ' of its first two digits.'), PHP_EOL;
  return false;
}
  if (!luhn_validation($cc)) {
  echo rtrim($error_message . ' it fails the Luhn check.'), PHP_EOL;
  return false;
}
  echo rtrim($cc . ' is a valid credit card number.'), PHP_EOL;
  return true;
}
function main() {
  validate_credit_card_number('4111111111111111');
  validate_credit_card_number('32323');
}
main();
