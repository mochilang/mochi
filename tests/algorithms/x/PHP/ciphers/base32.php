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
function repeat($s, $n) {
    return str_repeat($s, intval($n));
}
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$B32_CHARSET = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ234567';
function indexOfChar($s, $ch) {
  global $B32_CHARSET;
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
  global $B32_CHARSET;
  $upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $lower = 'abcdefghijklmnopqrstuvwxyz';
  $digits = '0123456789';
  $idx = indexOfChar($upper, $ch);
  if ($idx >= 0) {
  return 65 + $idx;
}
  $idx = indexOfChar($lower, $ch);
  if ($idx >= 0) {
  return 97 + $idx;
}
  $idx = indexOfChar($digits, $ch);
  if ($idx >= 0) {
  return 48 + $idx;
}
  if ($ch == ' ') {
  return 32;
}
  if ($ch == '!') {
  return 33;
}
  return 0;
}
function mochi_chr($code) {
  global $B32_CHARSET;
  $upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $lower = 'abcdefghijklmnopqrstuvwxyz';
  $digits = '0123456789';
  if ($code == 32) {
  return ' ';
}
  if ($code == 33) {
  return '!';
}
  $idx = $code - 65;
  if ($idx >= 0 && $idx < strlen($upper)) {
  return substr($upper, $idx, $idx + 1 - $idx);
}
  $idx = $code - 97;
  if ($idx >= 0 && $idx < strlen($lower)) {
  return substr($lower, $idx, $idx + 1 - $idx);
}
  $idx = $code - 48;
  if ($idx >= 0 && $idx < strlen($digits)) {
  return substr($digits, $idx, $idx + 1 - $idx);
}
  return '';
}
function mochi_repeat($s, $n) {
  global $B32_CHARSET;
  $out = '';
  $i = 0;
  while ($i < $n) {
  $out = $out . $s;
  $i = $i + 1;
};
  return $out;
}
function to_binary($n, $bits) {
  global $B32_CHARSET;
  $v = $n;
  $out = '';
  $i = 0;
  while ($i < $bits) {
  $out = _str($v % 2) . $out;
  $v = _intdiv($v, 2);
  $i = $i + 1;
};
  return $out;
}
function binary_to_int($bits) {
  global $B32_CHARSET;
  $n = 0;
  $i = 0;
  while ($i < strlen($bits)) {
  $n = $n * 2;
  if (substr($bits, $i, $i + 1 - $i) == '1') {
  $n = $n + 1;
}
  $i = $i + 1;
};
  return $n;
}
function base32_encode($data) {
  global $B32_CHARSET;
  $binary_data = '';
  $i = 0;
  while ($i < strlen($data)) {
  $binary_data = $binary_data . to_binary(mochi_ord(substr($data, $i, $i + 1 - $i)), 8);
  $i = $i + 1;
};
  $remainder = fmod(strlen($binary_data), 5);
  if ($remainder != 0) {
  $binary_data = $binary_data . repeat('0', 5 - $remainder);
}
  $b32_result = '';
  $j = 0;
  while ($j < strlen($binary_data)) {
  $chunk = substr($binary_data, $j, $j + 5 - $j);
  $index = binary_to_int($chunk);
  $b32_result = $b32_result . substr($B32_CHARSET, $index, $index + 1 - $index);
  $j = $j + 5;
};
  $rem = fmod(strlen($b32_result), 8);
  if ($rem != 0) {
  $b32_result = $b32_result . repeat('=', 8 - $rem);
}
  return $b32_result;
}
function base32_decode($data) {
  global $B32_CHARSET;
  $clean = '';
  $i = 0;
  while ($i < strlen($data)) {
  $ch = substr($data, $i, $i + 1 - $i);
  if ($ch != '=') {
  $clean = $clean . $ch;
}
  $i = $i + 1;
};
  $binary_chunks = '';
  $i = 0;
  while ($i < strlen($clean)) {
  $idx = indexOfChar($B32_CHARSET, substr($clean, $i, $i + 1 - $i));
  $binary_chunks = $binary_chunks . to_binary($idx, 5);
  $i = $i + 1;
};
  $result = '';
  $j = 0;
  while ($j + 8 <= strlen($binary_chunks)) {
  $byte_bits = substr($binary_chunks, $j, $j + 8 - $j);
  $code = binary_to_int($byte_bits);
  $result = $result . mochi_chr($code);
  $j = $j + 8;
};
  return $result;
}
echo rtrim(base32_encode('Hello World!')), PHP_EOL;
echo rtrim(base32_encode('123456')), PHP_EOL;
echo rtrim(base32_encode('some long complex string')), PHP_EOL;
echo rtrim(base32_decode('JBSWY3DPEBLW64TMMQQQ====')), PHP_EOL;
echo rtrim(base32_decode('GEZDGNBVGY======')), PHP_EOL;
echo rtrim(base32_decode('ONXW2ZJANRXW4ZZAMNXW24DMMV4CA43UOJUW4ZY=')), PHP_EOL;
