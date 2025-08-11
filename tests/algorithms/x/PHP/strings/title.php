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
$lower = 'abcdefghijklmnopqrstuvwxyz';
$upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
function index_of($s, $ch) {
  global $lower, $upper;
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = _iadd($i, 1);
};
  return -1;
}
function to_title_case($word) {
  global $lower, $upper;
  if (strlen($word) == 0) {
  return '';
}
  $first = substr($word, 0, 1);
  $idx = index_of($lower, $first);
  $result = ($idx >= 0 ? substr($upper, $idx, _iadd($idx, 1) - $idx) : $first);
  $i = 1;
  while ($i < strlen($word)) {
  $ch = substr($word, $i, _iadd($i, 1) - $i);
  $uidx = index_of($upper, $ch);
  if ($uidx >= 0) {
  $result = $result . substr($lower, $uidx, _iadd($uidx, 1) - $uidx);
} else {
  $result = $result . $ch;
}
  $i = _iadd($i, 1);
};
  return $result;
}
function split_words($s) {
  global $lower, $upper;
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
function sentence_to_title_case($sentence) {
  global $lower, $upper;
  $words = split_words($sentence);
  $res = '';
  $i = 0;
  while ($i < count($words)) {
  $res = $res . to_title_case($words[$i]);
  if (_iadd($i, 1) < count($words)) {
  $res = $res . ' ';
}
  $i = _iadd($i, 1);
};
  return $res;
}
echo rtrim(to_title_case('Aakash')), PHP_EOL;
echo rtrim(to_title_case('aakash')), PHP_EOL;
echo rtrim(to_title_case('AAKASH')), PHP_EOL;
echo rtrim(to_title_case('aAkAsH')), PHP_EOL;
echo rtrim(sentence_to_title_case('Aakash Giri')), PHP_EOL;
echo rtrim(sentence_to_title_case('aakash giri')), PHP_EOL;
echo rtrim(sentence_to_title_case('AAKASH GIRI')), PHP_EOL;
echo rtrim(sentence_to_title_case('aAkAsH gIrI')), PHP_EOL;
