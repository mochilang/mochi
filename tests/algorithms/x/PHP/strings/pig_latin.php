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
$VOWELS = 'aeiou';
function strip($s) {
  global $VOWELS;
  $start = 0;
  $end = strlen($s);
  while ($start < $end && substr($s, $start, _iadd($start, 1) - $start) == ' ') {
  $start = _iadd($start, 1);
};
  while ($end > $start && substr($s, _isub($end, 1), $end - _isub($end, 1)) == ' ') {
  $end = _isub($end, 1);
};
  return substr($s, $start, $end - $start);
}
function is_vowel($c) {
  global $VOWELS;
  $i = 0;
  while ($i < strlen($VOWELS)) {
  if ($c == substr($VOWELS, $i, _iadd($i, 1) - $i)) {
  return true;
}
  $i = _iadd($i, 1);
};
  return false;
}
function pig_latin($word) {
  global $VOWELS;
  $trimmed = strip($word);
  if (strlen($trimmed) == 0) {
  return '';
}
  $w = strtolower($trimmed);
  $first = substr($w, 0, 1);
  if (is_vowel($first)) {
  return $w . 'way';
}
  $i = 0;
  while ($i < strlen($w)) {
  $ch = substr($w, $i, _iadd($i, 1) - $i);
  if (is_vowel($ch)) {
  break;
}
  $i = _iadd($i, 1);
};
  return substr($w, $i, strlen($w) - $i) . substr($w, 0, $i) . 'ay';
}
echo rtrim('pig_latin(\'friends\') = ' . pig_latin('friends')), PHP_EOL;
echo rtrim('pig_latin(\'smile\') = ' . pig_latin('smile')), PHP_EOL;
echo rtrim('pig_latin(\'eat\') = ' . pig_latin('eat')), PHP_EOL;
