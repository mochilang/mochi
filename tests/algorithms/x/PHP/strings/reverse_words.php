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
function split_words($s) {
  $words = [];
  $current = '';
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch == ' ') {
  if (strlen($current) > 0) {
  $words = _append($words, $current);
  $current = '';
};
} else {
  $current = $current . $ch;
}
  $i = _iadd($i, 1);
};
  if (strlen($current) > 0) {
  $words = _append($words, $current);
}
  return $words;
}
function reverse_words($input_str) {
  $words = split_words($input_str);
  $res = '';
  $i = _isub(count($words), 1);
  while ($i >= 0) {
  $res = $res . $words[$i];
  if ($i > 0) {
  $res = $res . ' ';
}
  $i = _isub($i, 1);
};
  return $res;
}
function main() {
  echo rtrim(reverse_words('I love Python')), PHP_EOL;
  echo rtrim(reverse_words('I     Love          Python')), PHP_EOL;
}
main();
