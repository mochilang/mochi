<?php
error_reporting(E_ALL & ~E_DEPRECATED);
ini_set('memory_limit', '-1');
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
function int_sqrt($n) {
  $r = 0;
  while (($r + 1) * ($r + 1) <= $n) {
  $r = $r + 1;
};
  return $r;
}
function is_pronic($n) {
  if ($n < 0) {
  return false;
}
  if ($n % 2 != 0) {
  return false;
}
  $root = int_sqrt($n);
  return $n == $root * ($root + 1);
}
function test_is_pronic() {
  if (is_pronic(-1)) {
  _panic('-1 should not be pronic');
}
  if (!is_pronic(0)) {
  _panic('0 should be pronic');
}
  if (!is_pronic(2)) {
  _panic('2 should be pronic');
}
  if (is_pronic(5)) {
  _panic('5 should not be pronic');
}
  if (!is_pronic(6)) {
  _panic('6 should be pronic');
}
  if (is_pronic(8)) {
  _panic('8 should not be pronic');
}
  if (!is_pronic(30)) {
  _panic('30 should be pronic');
}
  if (is_pronic(32)) {
  _panic('32 should not be pronic');
}
  if (!is_pronic(2147441940)) {
  _panic('2147441940 should be pronic');
}
}
function main() {
  test_is_pronic();
  echo rtrim(json_encode(is_pronic(56), 1344)), PHP_EOL;
}
main();
