<?php
ini_set('memory_limit', '-1');
$ascii_chars = ' !"#$%&\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~';
function mochi_ord($ch) {
  global $ascii_chars;
  $i = 0;
  while ($i < strlen($ascii_chars)) {
  if (substr($ascii_chars, $i, $i + 1 - $i) == $ch) {
  return 32 + $i;
}
  $i = $i + 1;
};
  return 0;
}
function fletcher16($text) {
  global $ascii_chars;
  $sum1 = 0;
  $sum2 = 0;
  $i = 0;
  while ($i < strlen($text)) {
  $code = mochi_ord(substr($text, $i, $i + 1 - $i));
  $sum1 = ($sum1 + $code) % 255;
  $sum2 = ($sum1 + $sum2) % 255;
  $i = $i + 1;
};
  return $sum2 * 256 + $sum1;
}
