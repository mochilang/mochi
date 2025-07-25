<?php
ini_set('memory_limit', '-1');
function _indexof($s, $sub) {
    $pos = strpos($s, $sub);
    return $pos === false ? -1 : $pos;
}
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : sprintf('%.0f', $a);
        $sb = is_int($b) ? strval($b) : sprintf('%.0f', $b);
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
function mochi_xor($a, $b) {
  global $table;
  $res = 0;
  $bit = 1;
  $x = $a;
  $y = $b;
  while ($x > 0 || $y > 0) {
  $abit = $x % 2;
  $bbit = $y % 2;
  if ($abit != $bbit) {
  $res = $res + $bit;
}
  $x = _intdiv($x, 2);
  $y = _intdiv($y, 2);
  $bit = $bit * 2;
};
  return $res;
}
function rshift($x, $n) {
  global $table;
  $v = $x;
  $i = 0;
  while ($i < $n) {
  $v = _intdiv($v, 2);
  $i = $i + 1;
};
  return $v;
}
function mochi_ord($ch) {
  global $table;
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
  if ($ch == ' ') {
  return 32;
}
  return 0;
}
function toHex($n) {
  global $table;
  $digits = '0123456789ABCDEF';
  if ($n == 0) {
  return '0';
}
  $v = $n;
  $out = '';
  while ($v > 0) {
  $d = $v % 16;
  $out = substr($digits, $d, $d + 1 - $d) . $out;
  $v = _intdiv($v, 16);
};
  return $out;
}
function crc32Table() {
  $table = [];
  $i = 0;
  while ($i < 256) {
  $word = $i;
  $j = 0;
  while ($j < 8) {
  if ($word % 2 == 1) {
  $word = mochi_xor(rshift($word, 1), 3988292384);
} else {
  $word = rshift($word, 1);
}
  $j = $j + 1;
};
  $table = array_merge($table, [$word]);
  $i = $i + 1;
};
  return $table;
}
$table = crc32Table();
function mochi_crc32($s) {
  global $table;
  $crc = 4294967295;
  $i = 0;
  while ($i < strlen($s)) {
  $c = mochi_ord(substr($s, $i, $i + 1 - $i));
  $idx = mochi_xor($crc % 256, $c);
  $crc = mochi_xor($table[$idx], rshift($crc, 8));
  $i = $i + 1;
};
  return 4294967295 - $crc;
}
function main() {
  global $table;
  $s = 'The quick brown fox jumps over the lazy dog';
  $result = mochi_crc32($s);
  $hex = toHex($result);
  echo rtrim($hex), PHP_EOL;
}
main();
