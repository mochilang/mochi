<?php
error_reporting(E_ALL & ~E_DEPRECATED);
ini_set('memory_limit', '-1');
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
        $q = bcdiv($sa, $sb, 0);
        $rem = bcmod($sa, $sb);
        $neg = ((strpos($sa, '-') === 0) xor (strpos($sb, '-') === 0));
        if ($neg && bccomp($rem, '0') != 0) {
            $q = bcsub($q, '1');
        }
        return intval($q);
    }
    $ai = intval($a);
    $bi = intval($b);
    $q = intdiv($ai, $bi);
    if ((($ai ^ $bi) < 0) && ($ai % $bi != 0)) {
        $q -= 1;
    }
    return $q;
}
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
function mochi_panic($msg) {
}
function char_to_value($c) {
  $digits = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $i = 0;
  while ($i < strlen($digits)) {
  if (substr($digits, $i, $i + 1 - $i) == $c) {
  return $i;
}
  $i = $i + 1;
};
  _panic('invalid digit');
}
function int_to_base($number, $base) {
  if ($base < 2 || $base > 36) {
  _panic('\'base\' must be between 2 and 36 inclusive');
}
  if ($number < 0) {
  _panic('number must be a positive integer');
}
  $digits = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $n = $number;
  $result = '';
  while ($n > 0) {
  $remainder = $n % $base;
  $result = substr($digits, $remainder, $remainder + 1 - $remainder) . $result;
  $n = _intdiv($n, $base);
};
  if ($result == '') {
  $result = '0';
}
  return $result;
}
function base_to_int($num_str, $base) {
  $value = 0;
  $i = 0;
  while ($i < strlen($num_str)) {
  $c = substr($num_str, $i, $i + 1 - $i);
  $value = $value * $base + char_to_value($c);
  $i = $i + 1;
};
  return $value;
}
function sum_of_digits($num, $base) {
  if ($base < 2 || $base > 36) {
  _panic('\'base\' must be between 2 and 36 inclusive');
}
  $num_str = int_to_base($num, $base);
  $total = 0;
  $i = 0;
  while ($i < strlen($num_str)) {
  $c = substr($num_str, $i, $i + 1 - $i);
  $total = $total + char_to_value($c);
  $i = $i + 1;
};
  return int_to_base($total, $base);
}
function harshad_numbers_in_base($limit, $base) {
  if ($base < 2 || $base > 36) {
  _panic('\'base\' must be between 2 and 36 inclusive');
}
  if ($limit < 0) {
  return [];
}
  $numbers = [];
  $i = 1;
  while ($i < $limit) {
  $s = sum_of_digits($i, $base);
  $divisor = base_to_int($s, $base);
  if ($i % $divisor == 0) {
  $numbers = _append($numbers, int_to_base($i, $base));
}
  $i = $i + 1;
};
  return $numbers;
}
function is_harshad_number_in_base($num, $base) {
  if ($base < 2 || $base > 36) {
  _panic('\'base\' must be between 2 and 36 inclusive');
}
  if ($num < 0) {
  return false;
}
  $n = int_to_base($num, $base);
  $d = sum_of_digits($num, $base);
  $n_val = base_to_int($n, $base);
  $d_val = base_to_int($d, $base);
  return $n_val % $d_val == 0;
}
function main() {
  echo rtrim(int_to_base(0, 21)), PHP_EOL;
  echo rtrim(int_to_base(23, 2)), PHP_EOL;
  echo rtrim(int_to_base(58, 5)), PHP_EOL;
  echo rtrim(int_to_base(167, 16)), PHP_EOL;
  echo rtrim(sum_of_digits(103, 12)), PHP_EOL;
  echo rtrim(sum_of_digits(1275, 4)), PHP_EOL;
  echo rtrim(sum_of_digits(6645, 2)), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(harshad_numbers_in_base(15, 2), 1344)))))), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(harshad_numbers_in_base(12, 34), 1344)))))), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(harshad_numbers_in_base(12, 4), 1344)))))), PHP_EOL;
  echo rtrim(json_encode(is_harshad_number_in_base(18, 10), 1344)), PHP_EOL;
  echo rtrim(json_encode(is_harshad_number_in_base(21, 10), 1344)), PHP_EOL;
  echo rtrim(json_encode(is_harshad_number_in_base(-21, 5), 1344)), PHP_EOL;
}
main();
