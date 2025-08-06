<?php
ini_set('memory_limit', '-1');
$ASCII_UPPERCASE = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
$ASCII_LOWERCASE = 'abcdefghijklmnopqrstuvwxyz';
$NEG_ONE = 0 - 1;
function index_of($s, $ch) {
  global $ASCII_UPPERCASE, $ASCII_LOWERCASE, $NEG_ONE;
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return $NEG_ONE;
}
function to_uppercase($s) {
  global $ASCII_UPPERCASE, $ASCII_LOWERCASE, $NEG_ONE;
  $result = '';
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  $idx = index_of($ASCII_LOWERCASE, $ch);
  if ($idx == $NEG_ONE) {
  $result = $result . $ch;
} else {
  $result = $result . substr($ASCII_UPPERCASE, $idx, $idx + 1 - $idx);
}
  $i = $i + 1;
};
  return $result;
}
function gronsfeld($text, $key) {
  global $ASCII_UPPERCASE, $ASCII_LOWERCASE, $NEG_ONE;
  $ascii_len = strlen($ASCII_UPPERCASE);
  $key_len = strlen($key);
  if ($key_len == 0) {
  $panic('integer modulo by zero');
}
  $upper_text = to_uppercase($text);
  $encrypted = '';
  $i = 0;
  while ($i < strlen($upper_text)) {
  $ch = substr($upper_text, $i, $i + 1 - $i);
  $idx = index_of($ASCII_UPPERCASE, $ch);
  if ($idx == $NEG_ONE) {
  $encrypted = $encrypted . $ch;
} else {
  $key_idx = $i % $key_len;
  $shift = intval(substr($key, $key_idx, $key_idx + 1 - $key_idx));
  $new_position = ($idx + $shift) % $ascii_len;
  $encrypted = $encrypted . substr($ASCII_UPPERCASE, $new_position, $new_position + 1 - $new_position);
}
  $i = $i + 1;
};
  return $encrypted;
}
echo rtrim(gronsfeld('hello', '412')), PHP_EOL;
echo rtrim(gronsfeld('hello', '123')), PHP_EOL;
echo rtrim(gronsfeld('', '123')), PHP_EOL;
echo rtrim(gronsfeld('yes, ¥€$ - _!@#%?', '0')), PHP_EOL;
echo rtrim(gronsfeld('yes, ¥€$ - _!@#%?', '01')), PHP_EOL;
echo rtrim(gronsfeld('yes, ¥€$ - _!@#%?', '012')), PHP_EOL;
