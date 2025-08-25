<?php
error_reporting(E_ALL & ~E_DEPRECATED);
ini_set('memory_limit', '-1');
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
function is_prime($n) {
  if ($n < 2) {
  return false;
}
  if ($n % 2 == 0) {
  return $n == 2;
}
  $i = 3;
  while ($i * $i <= $n) {
  if ($n % $i == 0) {
  return false;
}
  $i = $i + 2;
};
  return true;
}
function twin_prime($number) {
  if (is_prime($number) && is_prime($number + 2)) {
  return $number + 2;
}
  return -1;
}
function test_twin_prime() {
  if (twin_prime(3) != 5) {
  _panic('twin_prime(3) failed');
}
  if (twin_prime(4) != (-1)) {
  _panic('twin_prime(4) failed');
}
  if (twin_prime(5) != 7) {
  _panic('twin_prime(5) failed');
}
  if (twin_prime(17) != 19) {
  _panic('twin_prime(17) failed');
}
  if (twin_prime(0) != (-1)) {
  _panic('twin_prime(0) failed');
}
}
function main() {
  test_twin_prime();
  echo rtrim(json_encode(twin_prime(3), 1344)), PHP_EOL;
}
main();
