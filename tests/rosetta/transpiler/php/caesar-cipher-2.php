<?php
ini_set('memory_limit', '-1');
function _str($x) {
    if (is_array($x)) {
        $isList = array_keys($x) === range(0, count($x) - 1);
        if ($isList) {
            $parts = [];
            foreach ($x as $v) { $parts[] = _str($v); }
            return '[' . implode(' ', $parts) . ']';
        }
        $parts = [];
        foreach ($x as $k => $v) { $parts[] = _str($k) . ':' . _str($v); }
        return 'map[' . implode(' ', $parts) . ']';
    }
    if (is_bool($x)) return $x ? 'true' : 'false';
    if ($x === null) return 'null';
    return strval($x);
}
function _indexof($s, $sub) {
    $pos = strpos($s, $sub);
    return $pos === false ? -1 : $pos;
}
function indexOf($s, $ch) {
  global $mochi_ord, $mochi_chr, $shiftRune, $encipher, $decipher, $main;
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
  global $indexOf, $mochi_chr, $shiftRune, $encipher, $decipher, $main;
  $upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $lower = 'abcdefghijklmnopqrstuvwxyz';
  $idx = _indexof($upper, $ch);
  if ($idx >= 0) {
  return 65 + $idx;
}
  $idx = _indexof($lower, $ch);
  if ($idx >= 0) {
  return 97 + $idx;
}
  return 0;
}
function mochi_chr($n) {
  global $indexOf, $mochi_ord, $shiftRune, $encipher, $decipher, $main;
  $upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $lower = 'abcdefghijklmnopqrstuvwxyz';
  if ($n >= 65 && $n < 91) {
  return substr($upper, $n - 65, $n - 64 - $n - 65);
}
  if ($n >= 97 && $n < 123) {
  return substr($lower, $n - 97, $n - 96 - $n - 97);
}
  return '?';
}
function shiftRune($r, $k) {
  global $indexOf, $mochi_ord, $mochi_chr, $encipher, $decipher, $main;
  if ($r >= 'a' && $r <= 'z') {
  return mochi_chr(((mochi_ord($r) - 97 + $k) % 26) + 97);
}
  if ($r >= 'A' && $r <= 'Z') {
  return mochi_chr(((mochi_ord($r) - 65 + $k) % 26) + 65);
}
  return $r;
}
function encipher($s, $k) {
  global $indexOf, $mochi_ord, $mochi_chr, $shiftRune, $decipher, $main;
  $out = '';
  $i = 0;
  while ($i < strlen($s)) {
  $out = $out . shiftRune(substr($s, $i, $i + 1 - $i), $k);
  $i = $i + 1;
};
  return $out;
}
function decipher($s, $k) {
  global $indexOf, $mochi_ord, $mochi_chr, $shiftRune, $encipher, $main;
  return encipher($s, (26 - $k % 26) % 26);
}
function main() {
  global $indexOf, $mochi_ord, $mochi_chr, $shiftRune, $encipher, $decipher;
  $pt = 'The five boxing wizards jump quickly';
  echo rtrim('Plaintext: ' . $pt), PHP_EOL;
  foreach ([0, 1, 7, 25, 26] as $key) {
  if ($key < 1 || $key > 25) {
  echo rtrim('Key ' . _str($key) . ' invalid'), PHP_EOL;
  continue;
}
  $ct = encipher($pt, $key);
  echo rtrim('Key ' . _str($key)), PHP_EOL;
  echo rtrim('  Enciphered: ' . $ct), PHP_EOL;
  echo rtrim('  Deciphered: ' . decipher($ct, $key)), PHP_EOL;
};
}
main();
