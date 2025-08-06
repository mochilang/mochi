<?php
ini_set('memory_limit', '-1');
function index_of($s, $ch) {
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
}
function mochi_ord($ch) {
  $upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $lower = 'abcdefghijklmnopqrstuvwxyz';
  $digits = '0123456789';
  $idx = index_of($upper, $ch);
  if ($idx >= 0) {
  return 65 + $idx;
}
  $idx = index_of($lower, $ch);
  if ($idx >= 0) {
  return 97 + $idx;
}
  $idx = index_of($digits, $ch);
  if ($idx >= 0) {
  return 48 + $idx;
}
  if ($ch == ' ') {
  return 32;
}
  return 0;
}
function djb2($s) {
  $hash_value = 5381;
  $i = 0;
  while ($i < strlen($s)) {
  $hash_value = $hash_value * 33 + mochi_ord(substr($s, $i, $i + 1 - $i));
  $hash_value = $hash_value % 4294967296;
  $i = $i + 1;
};
  return $hash_value;
}
echo rtrim(json_encode(djb2('Algorithms'), 1344)), PHP_EOL;
echo rtrim(json_encode(djb2('scramble bits'), 1344)), PHP_EOL;
