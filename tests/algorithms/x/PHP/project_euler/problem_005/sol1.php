<?php
ini_set('memory_limit', '-1');
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
function gcd($a, $b) {
  $x = $a;
  $y = $b;
  while ($y != 0) {
  $t = $x % $y;
  $x = $y;
  $y = $t;
};
  if ($x < 0) {
  return -$x;
}
  return $x;
}
function lcm($a, $b) {
  return $a / gcd($a, $b) * $b;
}
function solution($n) {
  if ($n <= 0) {
  _panic('Parameter n must be greater than or equal to one.');
}
  $result = 1;
  $i = 2;
  while ($i <= $n) {
  $result = lcm($result, $i);
  $i = $i + 1;
};
  return $result;
}
echo rtrim(json_encode(solution(10), 1344)), PHP_EOL;
echo rtrim(json_encode(solution(15), 1344)), PHP_EOL;
echo rtrim(json_encode(solution(22), 1344)), PHP_EOL;
echo rtrim(json_encode(solution(20), 1344)), PHP_EOL;
