<?php
ini_set('memory_limit','-1');
function mochi_pow($base, $exp) {
  global $ackermann2, $main;
  $result = 1;
  $i = 0;
  while ($i < $exp) {
  $result = $result * $base;
  $i = $i + 1;
};
  return $result;
}
function ackermann2($m, $n) {
  global $mochi_pow, $main;
  if ($m == 0) {
  return $n + 1;
}
  if ($m == 1) {
  return $n + 2;
}
  if ($m == 2) {
  return 2 * $n + 3;
}
  if ($m == 3) {
  return 8 * mochi_pow(2, $n) - 3;
}
  if ($n == 0) {
  return ackermann2($m - 1, 1);
}
  return ackermann2($m - 1, ackermann2($m, $n - 1));
}
function main() {
  global $mochi_pow, $ackermann2;
  echo "A(0, 0) = " . json_encode(ackermann2(0, 0), 1344), PHP_EOL;
  echo "A(1, 2) = " . json_encode(ackermann2(1, 2), 1344), PHP_EOL;
  echo "A(2, 4) = " . json_encode(ackermann2(2, 4), 1344), PHP_EOL;
  echo "A(3, 4) = " . json_encode(ackermann2(3, 4), 1344), PHP_EOL;
}
main();
