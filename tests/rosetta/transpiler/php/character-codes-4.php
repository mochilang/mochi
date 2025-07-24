<?php
ini_set('memory_limit', '-1');
function mochi_chr($n) {
  global $b, $r;
  if ($n == 97) {
  return 'a';
}
  if ($n == 960) {
  return 'π';
}
  if ($n == 65) {
  return 'A';
}
  return '?';
}
$b = 97;
$r = 960;
echo rtrim(mochi_chr(97) . ' ' . mochi_chr(960)), PHP_EOL;
echo rtrim(mochi_chr($b) . ' ' . mochi_chr($r)), PHP_EOL;
