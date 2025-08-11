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
$LETTERS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
$LOWERCASE = 'abcdefghijklmnopqrstuvwxyz';
function char_to_lower($c) {
  global $LETTERS, $LOWERCASE;
  $i = 0;
  while ($i < strlen($LETTERS)) {
  if ($c == substr($LETTERS, $i, _iadd($i, 1) - $i)) {
  return substr($LOWERCASE, $i, _iadd($i, 1) - $i);
}
  $i = _iadd($i, 1);
};
  return $c;
}
function normalize($input_str) {
  global $LETTERS, $LOWERCASE;
  $res = '';
  $i = 0;
  while ($i < strlen($input_str)) {
  $ch = substr($input_str, $i, _iadd($i, 1) - $i);
  $lc = char_to_lower($ch);
  if ($lc >= 'a' && $lc <= 'z') {
  $res = $res . $lc;
}
  $i = _iadd($i, 1);
};
  return $res;
}
function can_string_be_rearranged_as_palindrome_counter($input_str) {
  global $LETTERS, $LOWERCASE;
  $s = normalize($input_str);
  $freq = [];
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, _iadd($i, 1) - $i);
  if (array_key_exists($ch, $freq)) {
  $freq[$ch] = _iadd($freq[$ch], 1);
} else {
  $freq[$ch] = 1;
}
  $i = _iadd($i, 1);
};
  $odd = 0;
  foreach (array_keys($freq) as $key) {
  if (_imod($freq[$key], 2) != 0) {
  $odd = _iadd($odd, 1);
}
};
  return $odd < 2;
}
function can_string_be_rearranged_as_palindrome($input_str) {
  global $LETTERS, $LOWERCASE;
  $s = normalize($input_str);
  if (strlen($s) == 0) {
  return true;
}
  $character_freq_dict = [];
  $i = 0;
  while ($i < strlen($s)) {
  $character = substr($s, $i, _iadd($i, 1) - $i);
  if (array_key_exists($character, $character_freq_dict)) {
  $character_freq_dict[$character] = _iadd($character_freq_dict[$character], 1);
} else {
  $character_freq_dict[$character] = 1;
}
  $i = _iadd($i, 1);
};
  $odd_char = 0;
  foreach (array_keys($character_freq_dict) as $character_key) {
  $character_count = $character_freq_dict[$character_key];
  if (_imod($character_count, 2) != 0) {
  $odd_char = _iadd($odd_char, 1);
}
};
  return !($odd_char > 1);
}
echo rtrim(json_encode(can_string_be_rearranged_as_palindrome_counter('Momo'), 1344)), PHP_EOL;
echo rtrim(json_encode(can_string_be_rearranged_as_palindrome_counter('Mother'), 1344)), PHP_EOL;
echo rtrim(json_encode(can_string_be_rearranged_as_palindrome('Momo'), 1344)), PHP_EOL;
echo rtrim(json_encode(can_string_be_rearranged_as_palindrome('Mother'), 1344)), PHP_EOL;
