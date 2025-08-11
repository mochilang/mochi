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
function split($s, $sep) {
  $res = [];
  $current = '';
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, _iadd($i, 1) - $i);
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
function capitalize($word) {
  if (strlen($word) == 0) {
  return '';
}
  $first = strtoupper(substr($word, 0, 1));
  $rest = substr($word, 1, strlen($word) - 1);
  return $first . $rest;
}
function snake_to_camel_case($input_str, $use_pascal) {
  $words = split($input_str, '_');
  $result = '';
  $index = 0;
  if (!$use_pascal) {
  if (count($words) > 0) {
  $result = $words[0];
  $index = 1;
};
}
  while ($index < count($words)) {
  $word = $words[$index];
  $result = $result . capitalize($word);
  $index = _iadd($index, 1);
};
  return $result;
}
echo rtrim(snake_to_camel_case('some_random_string', false)), PHP_EOL;
echo rtrim(snake_to_camel_case('some_random_string', true)), PHP_EOL;
echo rtrim(snake_to_camel_case('some_random_string_with_numbers_123', false)), PHP_EOL;
echo rtrim(snake_to_camel_case('some_random_string_with_numbers_123', true)), PHP_EOL;
