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
  foreach ([-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10] as $i) {
  if ($i < 0) {
  echo "fib(" . json_encode($i, 1344) . ") returned error: negative n is forbidden", PHP_EOL;
} else {
  echo "fib(" . json_encode($i, 1344) . ") = " . json_encode(fib($i), 1344), PHP_EOL;
}
};
}
main();
