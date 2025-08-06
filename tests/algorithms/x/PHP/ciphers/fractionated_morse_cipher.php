<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$MORSE_CODE_DICT = ['A' => '.-', 'B' => '-...', 'C' => '-.-.', 'D' => '-..', 'E' => '.', 'F' => '..-.', 'G' => '--.', 'H' => '....', 'I' => '..', 'J' => '.---', 'K' => '-.-', 'L' => '.-..', 'M' => '--', 'N' => '-.', 'O' => '---', 'P' => '.--.', 'Q' => '--.-', 'R' => '.-.', 'S' => '...', 'T' => '-', 'U' => '..-', 'V' => '...-', 'W' => '.--', 'X' => '-..-', 'Y' => '-.--', 'Z' => '--..', ' ' => ''];
$MORSE_COMBINATIONS = ['...', '..-', '..x', '.-.', '.--', '.-x', '.x.', '.x-', '.xx', '-..', '-.-', '-.x', '--.', '---', '--x', '-x.', '-x-', '-xx', 'x..', 'x.-', 'x.x', 'x-.', 'x--', 'x-x', 'xx.', 'xx-', 'xxx'];
$REVERSE_DICT = ['.-' => 'A', '-...' => 'B', '-.-.' => 'C', '-..' => 'D', '.' => 'E', '..-.' => 'F', '--.' => 'G', '....' => 'H', '..' => 'I', '.---' => 'J', '-.-' => 'K', '.-..' => 'L', '--' => 'M', '-.' => 'N', '---' => 'O', '.--.' => 'P', '--.-' => 'Q', '.-.' => 'R', '...' => 'S', '-' => 'T', '..-' => 'U', '...-' => 'V', '.--' => 'W', '-..-' => 'X', '-.--' => 'Y', '--..' => 'Z', '' => ' '];
function encodeToMorse($plaintext) {
  global $MORSE_CODE_DICT, $MORSE_COMBINATIONS, $REVERSE_DICT, $key, $ciphertext, $decrypted;
  $morse = '';
  $i = 0;
  while ($i < strlen($plaintext)) {
  $ch = strtoupper(substr($plaintext, $i, $i + 1 - $i));
  $code = '';
  if (array_key_exists($ch, $MORSE_CODE_DICT)) {
  $code = $MORSE_CODE_DICT[$ch];
}
  if ($i > 0) {
  $morse = $morse . 'x';
}
  $morse = $morse . $code;
  $i = $i + 1;
};
  return $morse;
}
function encryptFractionatedMorse($plaintext, $key) {
  global $MORSE_CODE_DICT, $MORSE_COMBINATIONS, $REVERSE_DICT, $ciphertext, $decrypted;
  $morseCode = encodeToMorse($plaintext);
  $combinedKey = strtoupper($key) . 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $dedupKey = '';
  $i = 0;
  while ($i < strlen($combinedKey)) {
  $ch = substr($combinedKey, $i, $i + 1 - $i);
  if (!(strpos($dedupKey, $ch) !== false)) {
  $dedupKey = $dedupKey . $ch;
}
  $i = $i + 1;
};
  $paddingLength = 3 - (fmod(strlen($morseCode), 3));
  $p = 0;
  while ($p < $paddingLength) {
  $morseCode = $morseCode . 'x';
  $p = $p + 1;
};
  $dict = [];
  $j = 0;
  while ($j < 26) {
  $combo = $MORSE_COMBINATIONS[$j];
  $letter = substr($dedupKey, $j, $j + 1 - $j);
  $dict[$combo] = $letter;
  $j = $j + 1;
};
  $dict['xxx'] = '';
  $encrypted = '';
  $k = 0;
  while ($k < strlen($morseCode)) {
  $group = substr($morseCode, $k, $k + 3 - $k);
  $encrypted = $encrypted . $dict[$group];
  $k = $k + 3;
};
  return $encrypted;
}
function decryptFractionatedMorse($ciphertext, $key) {
  global $MORSE_CODE_DICT, $MORSE_COMBINATIONS, $REVERSE_DICT, $plaintext;
  $combinedKey = strtoupper($key) . 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $dedupKey = '';
  $i = 0;
  while ($i < strlen($combinedKey)) {
  $ch = substr($combinedKey, $i, $i + 1 - $i);
  if (!(strpos($dedupKey, $ch) !== false)) {
  $dedupKey = $dedupKey . $ch;
}
  $i = $i + 1;
};
  $inv = [];
  $j = 0;
  while ($j < 26) {
  $letter = substr($dedupKey, $j, $j + 1 - $j);
  $inv[$letter] = $MORSE_COMBINATIONS[$j];
  $j = $j + 1;
};
  $morse = '';
  $k = 0;
  while ($k < strlen($ciphertext)) {
  $ch = substr($ciphertext, $k, $k + 1 - $k);
  if (array_key_exists($ch, $inv)) {
  $morse = $morse . $inv[$ch];
}
  $k = $k + 1;
};
  $codes = [];
  $current = '';
  $m = 0;
  while ($m < strlen($morse)) {
  $ch = substr($morse, $m, $m + 1 - $m);
  if ($ch == 'x') {
  $codes = _append($codes, $current);
  $current = '';
} else {
  $current = $current . $ch;
}
  $m = $m + 1;
};
  $codes = _append($codes, $current);
  $decrypted = '';
  $idx = 0;
  while ($idx < count($codes)) {
  $code = $codes[$idx];
  $decrypted = $decrypted . $REVERSE_DICT[$code];
  $idx = $idx + 1;
};
  $start = 0;
  while (true) {
  if ($start < strlen($decrypted)) {
  if (substr($decrypted, $start, $start + 1 - $start) == ' ') {
  $start = $start + 1;
  continue;
};
}
  break;
};
  $end = strlen($decrypted);
  while (true) {
  if ($end > $start) {
  if (substr($decrypted, $end - 1, $end - ($end - 1)) == ' ') {
  $end = $end - 1;
  continue;
};
}
  break;
};
  return substr($decrypted, $start, $end - $start);
}
$plaintext = 'defend the east';
echo rtrim('Plain Text:') . " " . rtrim($plaintext), PHP_EOL;
$key = 'ROUNDTABLE';
$ciphertext = encryptFractionatedMorse($plaintext, $key);
echo rtrim('Encrypted:') . " " . rtrim($ciphertext), PHP_EOL;
$decrypted = decryptFractionatedMorse($ciphertext, $key);
echo rtrim('Decrypted:') . " " . rtrim($decrypted), PHP_EOL;
