<?php
ini_set('memory_limit', '-1');
$LOWER = 'abcdefghijklmnopqrstuvwxyz';
$UPPER = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
function to_lowercase($s) {
  global $LOWER, $UPPER;
  $res = '';
  $i = 0;
  while ($i < strlen($s)) {
  $c = substr($s, $i, $i + 1 - $i);
  $j = 0;
  $found = false;
  while ($j < 26) {
  if ($c == substr($UPPER, $j, $j + 1 - $j)) {
  $res = $res . substr($LOWER, $j, $j + 1 - $j);
  $found = true;
  break;
}
  $j = $j + 1;
};
  if (!$found) {
  $res = $res . $c;
}
  $i = $i + 1;
};
  return $res;
}
function char_index($c) {
  global $LOWER, $UPPER;
  $i = 0;
  while ($i < 26) {
  if ($c == substr($LOWER, $i, $i + 1 - $i)) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
}
function index_char($i) {
  global $LOWER, $UPPER;
  return substr($LOWER, $i, $i + 1 - $i);
}
function encrypt($plaintext, $key) {
  global $LOWER, $UPPER;
  if (strlen($plaintext) == 0) {
  $panic('plaintext is empty');
}
  if (strlen($key) == 0) {
  $panic('key is empty');
}
  $full_key = $key . $plaintext;
  $plaintext = to_lowercase($plaintext);
  $full_key = to_lowercase($full_key);
  $p_i = 0;
  $k_i = 0;
  $ciphertext = '';
  while ($p_i < strlen($plaintext)) {
  $p_char = substr($plaintext, $p_i, $p_i + 1 - $p_i);
  $p_idx = char_index($p_char);
  if ($p_idx < 0) {
  $ciphertext = $ciphertext . $p_char;
  $p_i = $p_i + 1;
} else {
  $k_char = substr($full_key, $k_i, $k_i + 1 - $k_i);
  $k_idx = char_index($k_char);
  if ($k_idx < 0) {
  $k_i = $k_i + 1;
} else {
  $c_idx = ($p_idx + $k_idx) % 26;
  $ciphertext = $ciphertext . index_char($c_idx);
  $k_i = $k_i + 1;
  $p_i = $p_i + 1;
};
}
};
  return $ciphertext;
}
function decrypt($ciphertext, $key) {
  global $LOWER, $UPPER;
  if (strlen($ciphertext) == 0) {
  $panic('ciphertext is empty');
}
  if (strlen($key) == 0) {
  $panic('key is empty');
}
  $current_key = to_lowercase($key);
  $c_i = 0;
  $k_i = 0;
  $plaintext = '';
  while ($c_i < strlen($ciphertext)) {
  $c_char = substr($ciphertext, $c_i, $c_i + 1 - $c_i);
  $c_idx = char_index($c_char);
  if ($c_idx < 0) {
  $plaintext = $plaintext . $c_char;
} else {
  $k_char = substr($current_key, $k_i, $k_i + 1 - $k_i);
  $k_idx = char_index($k_char);
  $p_idx = ($c_idx - $k_idx + 26) % 26;
  $p_char = index_char($p_idx);
  $plaintext = $plaintext . $p_char;
  $current_key = $current_key . $p_char;
  $k_i = $k_i + 1;
}
  $c_i = $c_i + 1;
};
  return $plaintext;
}
echo rtrim(encrypt('hello world', 'coffee')), PHP_EOL;
echo rtrim(decrypt('jsqqs avvwo', 'coffee')), PHP_EOL;
echo rtrim(encrypt('coffee is good as python', 'TheAlgorithms')), PHP_EOL;
echo rtrim(decrypt('vvjfpk wj ohvp su ddylsv', 'TheAlgorithms')), PHP_EOL;
