<?php
function countDivisors($n) {
  global $main;
  if ($n < 2) {
  return 1;
}
  $count = 2;
  $i = 2;
  while ($i <= intdiv($n, 2)) {
  if ($n % $i == 0) {
  $count = $count + 1;
}
  $i = $i + 1;
};
  return $count;
}
function main() {
  global $countDivisors;
  echo "The first 20 anti-primes are:", PHP_EOL;
  $maxDiv = 0;
  $count = 0;
  $n = 1;
  $line = "";
  while ($count < 20) {
  $d = countDivisors($n);
  if ($d > $maxDiv) {
  $line = $line . json_encode($n, 1344) . " ";
  $maxDiv = $d;
  $count = $count + 1;
}
  $n = $n + 1;
};
  $line = substr($line, 0, strlen($line) - 1 - 0);
  echo $line, PHP_EOL;
}
main();
