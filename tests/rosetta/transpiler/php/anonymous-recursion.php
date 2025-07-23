<?php
function fib($n) {
  global $main;
  if ($n < 2) {
  return $n;
}
  return fib($n - 1) + fib($n - 2);
}
function main() {
  global $fib;
  $i = -1;
  while ($i <= 10) {
  if ($i < 0) {
  echo "fib(" . json_encode($i, 1344) . ") returned error: negative n is forbidden", PHP_EOL;
} else {
  echo "fib(" . json_encode($i, 1344) . ") = " . json_encode(fib($i), 1344), PHP_EOL;
}
  $i = $i + 1;
};
}
main();
