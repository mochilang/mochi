<?php
function fib($n) {
  global $main;
  if ($n < 2) {
  return $n;
}
  $a = 0;
  $b = 1;
  $i = 1;
  while ($i < $n) {
  $t = $a + $b;
  $a = $b;
  $b = $t;
  $i = $i + 1;
};
  return $b;
}
function main() {
  global $fib;
  foreach ([0, 1, 2, 3, 4, 5, 10, 40, -1] as $n) {
  if ($n < 0) {
  echo "fib undefined for negative numbers", PHP_EOL;
} else {
  echo "fib " . json_encode($n, 1344) . " = " . json_encode(fib($n), 1344), PHP_EOL;
}
};
}
main();
