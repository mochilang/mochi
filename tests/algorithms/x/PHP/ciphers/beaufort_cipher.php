<?php
ini_set('memory_limit', '-1');
$ALPHABET = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
function index_of($ch) {
  global $ALPHABET, $message, $key, $key_new, $encrypted;
  for ($i = 0; $i < strlen($ALPHABET); $i++) {
  if (substr($ALPHABET, $i, $i + 1 - $i) == $ch) {
  return $i;
}
};
  return -1;
}
function generate_key($message, $key) {
  global $ALPHABET, $encrypted;
  $key_new = $key;
  $i = 0;
  while (strlen($key_new) < strlen($message)) {
  $key_new = $key_new . substr($key, $i, $i + 1 - $i);
  $i = $i + 1;
  if ($i == strlen($key)) {
  $i = 0;
}
};
  return $key_new;
}
function cipher_text($message, $key_new) {
  global $ALPHABET, $key, $encrypted;
  $res = '';
  $i = 0;
  for ($idx = 0; $idx < strlen($message); $idx++) {
  $ch = substr($message, $idx, $idx + 1 - $idx);
  if ($ch == ' ') {
  $res = $res . ' ';
} else {
  $x = fmod((index_of($ch) - index_of(substr($key_new, $i, $i + 1 - $i)) + 26), 26);
  $i = $i + 1;
  $res = $res . substr($ALPHABET, $x, $x + 1 - $x);
}
};
  return $res;
}
function original_text($cipher, $key_new) {
  global $ALPHABET, $message, $key, $encrypted;
  $res = '';
  $i = 0;
  for ($idx = 0; $idx < strlen($cipher); $idx++) {
  $ch = substr($cipher, $idx, $idx + 1 - $idx);
  if ($ch == ' ') {
  $res = $res . ' ';
} else {
  $x = fmod((index_of($ch) + index_of(substr($key_new, $i, $i + 1 - $i)) + 26), 26);
  $i = $i + 1;
  $res = $res . substr($ALPHABET, $x, $x + 1 - $x);
}
};
  return $res;
}
$message = 'THE GERMAN ATTACK';
$key = 'SECRET';
$key_new = generate_key($message, $key);
$encrypted = cipher_text($message, $key_new);
echo rtrim('Encrypted Text = ' . $encrypted), PHP_EOL;
echo rtrim('Original Text = ' . original_text($encrypted, $key_new)), PHP_EOL;
