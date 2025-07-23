<?php
function isPrime($n) {
  global $sumDigits, $pad, $main;
  if ($n < 2) {
  return false;
}
  if ($n % 2 == 0) {
  return $n == 2;
}
  if ($n % 3 == 0) {
  return $n == 3;
}
  $d = 5;
  while ($d * $d <= $n) {
  if ($n % $d == 0) {
  return false;
}
  $d = $d + 2;
  if ($n % $d == 0) {
  return false;
}
  $d = $d + 4;
};
  return true;
}
function sumDigits($n) {
  global $isPrime, $pad, $main;
  $s = 0;
  $x = $n;
  while ($x > 0) {
  $s = $s + $x % 10;
  $x = intval((intdiv($x, 10)));
};
  return $s;
}
function pad($n) {
  global $isPrime, $sumDigits, $main;
  if ($n < 10) {
  return "  " . json_encode($n, 1344);
}
  if ($n < 100) {
  return " " . json_encode($n, 1344);
}
  return json_encode($n, 1344);
}
function main() {
  global $isPrime, $sumDigits, $pad;
  echo "Additive primes less than 500:", PHP_EOL;
  $count = 0;
  $line = "";
  $lineCount = 0;
  $i = 2;
  while ($i < 500) {
  if (isPrime($i) && isPrime(sumDigits($i))) {
  $count = $count + 1;
  $line = $line . pad($i) . "  ";
  $lineCount = $lineCount + 1;
  if ($lineCount == 10) {
  echo json_encode(substr($line, 0, strlen($line) - 2 - 0), 1344), PHP_EOL;
  $line = "";
  $lineCount = 0;
};
}
  if ($i > 2) {
  $i = $i + 2;
} else {
  $i = $i + 1;
}
};
  if ($lineCount > 0) {
  echo json_encode(substr($line, 0, strlen($line) - 2 - 0), 1344), PHP_EOL;
}
  echo json_encode($count, 1344) . " additive primes found.", PHP_EOL;
}
main();
