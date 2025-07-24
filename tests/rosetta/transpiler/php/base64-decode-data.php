<?php
ini_set('memory_limit', '-1');
function indexOf($s, $ch) {
  global $parseIntStr, $mochi_ord, $mochi_chr, $toBinary, $binToInt, $base64Encode, $base64Decode, $msg, $enc, $dec;
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
}
function parseIntStr($str) {
  global $indexOf, $mochi_ord, $mochi_chr, $toBinary, $binToInt, $base64Encode, $base64Decode, $msg, $enc, $dec;
  $i = 0;
  $neg = false;
  if (strlen($str) > 0 && substr($str, 0, 0 + 1 - 0) == '-') {
  $neg = true;
  $i = 1;
}
  $n = 0;
  $digits = ['0' => 0, '1' => 1, '2' => 2, '3' => 3, '4' => 4, '5' => 5, '6' => 6, '7' => 7, '8' => 8, '9' => 9];
  while ($i < strlen($str)) {
  $n = $n * 10 + $digits[substr($str, $i, $i + 1 - $i)];
  $i = $i + 1;
};
  if ($neg) {
  $n = -$n;
}
  return $n;
}
function mochi_ord($ch) {
  global $indexOf, $parseIntStr, $mochi_chr, $toBinary, $binToInt, $base64Encode, $base64Decode, $msg, $enc, $dec;
  $upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $lower = 'abcdefghijklmnopqrstuvwxyz';
  $idx = indexOf($upper, $ch);
  if ($idx >= 0) {
  return 65 + $idx;
}
  $idx = indexOf($lower, $ch);
  if ($idx >= 0) {
  return 97 + $idx;
}
  if ($ch >= '0' && $ch <= '9') {
  return 48 + parseIntStr($ch);
}
  if ($ch == '+') {
  return 43;
}
  if ($ch == '/') {
  return 47;
}
  if ($ch == ' ') {
  return 32;
}
  if ($ch == '=') {
  return 61;
}
  return 0;
}
function mochi_chr($n) {
  global $indexOf, $parseIntStr, $mochi_ord, $toBinary, $binToInt, $base64Encode, $base64Decode, $msg, $enc, $dec;
  $upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $lower = 'abcdefghijklmnopqrstuvwxyz';
  if ($n >= 65 && $n < 91) {
  return substr($upper, $n - 65, $n - 64 - $n - 65);
}
  if ($n >= 97 && $n < 123) {
  return substr($lower, $n - 97, $n - 96 - $n - 97);
}
  if ($n >= 48 && $n < 58) {
  $digits = '0123456789';
  return substr($digits, $n - 48, $n - 47 - $n - 48);
}
  if ($n == 43) {
  return '+';
}
  if ($n == 47) {
  return '/';
}
  if ($n == 32) {
  return ' ';
}
  if ($n == 61) {
  return '=';
}
  return '?';
}
function toBinary($n, $bits) {
  global $indexOf, $parseIntStr, $mochi_ord, $mochi_chr, $binToInt, $base64Encode, $base64Decode, $msg, $enc, $dec;
  $b = '';
  $val = $n;
  $i = 0;
  while ($i < $bits) {
  $b = json_encode($val % 2, 1344) . $b;
  $val = intval((intdiv($val, 2)));
  $i = $i + 1;
};
  return $b;
}
function binToInt($bits) {
  global $indexOf, $parseIntStr, $mochi_ord, $mochi_chr, $toBinary, $base64Encode, $base64Decode, $msg, $enc, $dec;
  $n = 0;
  $i = 0;
  while ($i < strlen($bits)) {
  $n = $n * 2 + parseIntStr(substr($bits, $i, $i + 1 - $i));
  $i = $i + 1;
};
  return $n;
}
function base64Encode($text) {
  global $indexOf, $parseIntStr, $mochi_ord, $mochi_chr, $toBinary, $binToInt, $base64Decode, $msg, $enc, $dec;
  $alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  $bin = '';
  foreach ($text as $ch) {
  $bin = $bin . toBinary(mochi_ord($ch), 8);
};
  while (strlen($bin) % 6 != 0) {
  $bin = $bin . '0';
};
  $out = '';
  $i = 0;
  while ($i < strlen($bin)) {
  $chunk = substr($bin, $i, $i + 6 - $i);
  $val = binToInt($chunk);
  $out = $out . substr($alphabet, $val, $val + 1 - $val);
  $i = $i + 6;
};
  $pad = (3 - (strlen($text) % 3)) % 3;
  if ($pad == 1) {
  $out = substr($out, 0, strlen($out) - 1 - 0) . '=';
}
  if ($pad == 2) {
  $out = substr($out, 0, strlen($out) - 2 - 0) . '==';
}
  return $out;
}
function base64Decode($enc) {
  global $indexOf, $parseIntStr, $mochi_ord, $mochi_chr, $toBinary, $binToInt, $base64Encode, $msg, $dec;
  $alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  $bin = '';
  $i = 0;
  while ($i < strlen($enc)) {
  $ch = substr($enc, $i, $i + 1 - $i);
  if ($ch == '=') {
  break;
}
  $idx = indexOf($alphabet, $ch);
  $bin = $bin . toBinary($idx, 6);
  $i = $i + 1;
};
  $out = '';
  $i = 0;
  while ($i + 8 <= strlen($bin)) {
  $chunk = substr($bin, $i, $i + 8 - $i);
  $val = binToInt($chunk);
  $out = $out . mochi_chr($val);
  $i = $i + 8;
};
  return $out;
}
$msg = 'Rosetta Code Base64 decode data task';
echo rtrim('Original : ' . $msg), PHP_EOL;
$enc = base64Encode($msg);
echo rtrim('
Encoded  : ' . $enc), PHP_EOL;
$dec = base64Decode($enc);
echo rtrim('
Decoded  : ' . $dec), PHP_EOL;
