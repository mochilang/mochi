<?php
function ackermann($m, $n) {
  global $main;
  if ($m == 0) {
  return $n + 1;
}
  if ($n == 0) {
  return ackermann($m - 1, 1);
}
  return ackermann($m - 1, ackermann($m, $n - 1));
}
function main() {
  global $ackermann;
  echo "A(0, 0) = " . json_encode(ackermann(0, 0), 1344), PHP_EOL;
  echo "A(1, 2) = " . json_encode(ackermann(1, 2), 1344), PHP_EOL;
  echo "A(2, 4) = " . json_encode(ackermann(2, 4), 1344), PHP_EOL;
  echo "A(3, 4) = " . json_encode(ackermann(3, 4), 1344), PHP_EOL;
}
main();
