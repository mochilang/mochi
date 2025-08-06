<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$default_alphabet = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
function index_of($s, $ch) {
  global $default_alphabet;
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
}
function encrypt($input_string, $key, $alphabet) {
  global $default_alphabet;
  $result = '';
  $i = 0;
  $n = strlen($alphabet);
  while ($i < strlen($input_string)) {
  $ch = substr($input_string, $i, $i + 1 - $i);
  $idx = index_of($alphabet, $ch);
  if ($idx < 0) {
  $result = $result . $ch;
} else {
  $new_key = ($idx + $key) % $n;
  if ($new_key < 0) {
  $new_key = $new_key + $n;
};
  $result = $result . substr($alphabet, $new_key, $new_key + 1 - $new_key);
}
  $i = $i + 1;
};
  return $result;
}
function decrypt($input_string, $key, $alphabet) {
  global $default_alphabet;
  $result = '';
  $i = 0;
  $n = strlen($alphabet);
  while ($i < strlen($input_string)) {
  $ch = substr($input_string, $i, $i + 1 - $i);
  $idx = index_of($alphabet, $ch);
  if ($idx < 0) {
  $result = $result . $ch;
} else {
  $new_key = ($idx - $key) % $n;
  if ($new_key < 0) {
  $new_key = $new_key + $n;
};
  $result = $result . substr($alphabet, $new_key, $new_key + 1 - $new_key);
}
  $i = $i + 1;
};
  return $result;
}
function brute_force($input_string, $alphabet) {
  global $default_alphabet;
  $results = [];
  $key = 1;
  $n = strlen($alphabet);
  while ($key <= $n) {
  $message = decrypt($input_string, $key, $alphabet);
  $results = _append($results, $message);
  $key = $key + 1;
};
  return $results;
}
function main() {
  global $default_alphabet;
  $alpha = $default_alphabet;
  $enc = encrypt('The quick brown fox jumps over the lazy dog', 8, $alpha);
  echo rtrim($enc), PHP_EOL;
  $dec = decrypt($enc, 8, $alpha);
  echo rtrim($dec), PHP_EOL;
  $brute = brute_force('jFyuMy xIH\'N vLONy zILwy Gy!', $alpha);
  echo rtrim($brute[19]), PHP_EOL;
}
main();
