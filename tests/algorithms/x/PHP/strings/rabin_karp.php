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
$alphabet_size = 256;
$modulus = 1000003;
function index_of_char($s, $ch) {
  global $alphabet_size, $modulus;
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = _iadd($i, 1);
};
  return -1;
}
function mochi_ord($ch) {
  global $alphabet_size, $modulus;
  $upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $lower = 'abcdefghijklmnopqrstuvwxyz';
  $digits = '0123456789';
  $idx = index_of_char($upper, $ch);
  if ($idx >= 0) {
  return _iadd(65, $idx);
}
  $idx = index_of_char($lower, $ch);
  if ($idx >= 0) {
  return _iadd(97, $idx);
}
  $idx = index_of_char($digits, $ch);
  if ($idx >= 0) {
  return _iadd(48, $idx);
}
  if ($ch == 'ü') {
  return 252;
}
  if ($ch == 'Ü') {
  return 220;
}
  if ($ch == ' ') {
  return 32;
}
  return 0;
}
function rabin_karp($pattern, $text) {
  global $alphabet_size, $modulus;
  $p_len = strlen($pattern);
  $t_len = strlen($text);
  if ($p_len > $t_len) {
  return false;
}
  $p_hash = 0;
  $t_hash = 0;
  $modulus_power = 1;
  $i = 0;
  while ($i < $p_len) {
  $p_hash = _imod((_iadd(mochi_ord(substr($pattern, $i, $i + 1 - $i)), _imul($p_hash, $alphabet_size))), $modulus);
  $t_hash = _imod((_iadd(mochi_ord(substr($text, $i, $i + 1 - $i)), _imul($t_hash, $alphabet_size))), $modulus);
  if ($i != _isub($p_len, 1)) {
  $modulus_power = _imod((_imul($modulus_power, $alphabet_size)), $modulus);
}
  $i = _iadd($i, 1);
};
  $j = 0;
  while ($j <= _isub($t_len, $p_len)) {
  if ($t_hash == $p_hash && substr($text, $j, _iadd($j, $p_len) - $j) == $pattern) {
  return true;
}
  if ($j == _isub($t_len, $p_len)) {
  $j = _iadd($j, 1);
  continue;
}
  $t_hash = _imod((_iadd(_imul((_isub($t_hash, _imul(mochi_ord(substr($text, $j, $j + 1 - $j)), $modulus_power))), $alphabet_size), mochi_ord(substr($text, _iadd($j, $p_len), _iadd($j, $p_len) + 1 - _iadd($j, $p_len))))), $modulus);
  if ($t_hash < 0) {
  $t_hash = _iadd($t_hash, $modulus);
}
  $j = _iadd($j, 1);
};
  return false;
}
function test_rabin_karp() {
  global $alphabet_size, $modulus;
  $pattern1 = 'abc1abc12';
  $text1 = 'alskfjaldsabc1abc1abc12k23adsfabcabc';
  $text2 = 'alskfjaldsk23adsfabcabc';
  if (!rabin_karp($pattern1, $text1) || rabin_karp($pattern1, $text2)) {
  echo rtrim('Failure'), PHP_EOL;
  return;
}
  $pattern2 = 'ABABX';
  $text3 = 'ABABZABABYABABX';
  if (!rabin_karp($pattern2, $text3)) {
  echo rtrim('Failure'), PHP_EOL;
  return;
}
  $pattern3 = 'AAAB';
  $text4 = 'ABAAAAAB';
  if (!rabin_karp($pattern3, $text4)) {
  echo rtrim('Failure'), PHP_EOL;
  return;
}
  $pattern4 = 'abcdabcy';
  $text5 = 'abcxabcdabxabcdabcdabcy';
  if (!rabin_karp($pattern4, $text5)) {
  echo rtrim('Failure'), PHP_EOL;
  return;
}
  $pattern5 = 'Lü';
  $text6 = 'Lüsai';
  if (!rabin_karp($pattern5, $text6)) {
  echo rtrim('Failure'), PHP_EOL;
  return;
}
  $pattern6 = 'Lue';
  if (rabin_karp($pattern6, $text6)) {
  echo rtrim('Failure'), PHP_EOL;
  return;
}
  echo rtrim('Success.'), PHP_EOL;
}
test_rabin_karp();
