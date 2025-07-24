<?php
ini_set('memory_limit', '-1');
function mochi_chr($n) {
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
echo rtrim(json_encode(mochi_chr(97), 1344)), PHP_EOL;
echo rtrim(json_encode(mochi_chr(960), 1344)), PHP_EOL;
echo rtrim(mochi_chr(97) . mochi_chr(960)), PHP_EOL;
