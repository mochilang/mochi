<?php
ini_set('memory_limit', '-1');
function isPrime($n) {
  global $countPrimeFactors, $pad4, $main;
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
function countPrimeFactors($n) {
  global $isPrime, $pad4, $main;
  if ($n == 1) {
  return 0;
}
  if (isPrime($n)) {
  return 1;
}
  $count = 0;
  $f = 2;
  while (true) {
  if ($n % $f == 0) {
  $count = $count + 1;
  $n = intdiv($n, $f);
  if ($n == 1) {
  return $count;
};
  if (isPrime($n)) {
  $f = $n;
};
} else {
  if ($f >= 3) {
  $f = $f + 2;
} else {
  $f = 3;
};
}
};
  return $count;
}
function pad4($n) {
  global $isPrime, $countPrimeFactors, $main;
  $s = json_encode($n, 1344);
  while (strlen($s) < 4) {
  $s = ' ' . $s;
};
  return $s;
}
function main() {
  global $isPrime, $countPrimeFactors, $pad4;
  $max = 120;
  echo rtrim('The attractive numbers up to and including ' . json_encode($max, 1344) . ' are:'), PHP_EOL;
  $count = 0;
  $line = '';
  $lineCount = 0;
  $i = 1;
  while ($i <= $max) {
  $c = countPrimeFactors($i);
  if (isPrime($c)) {
  $line = $line . pad4($i);
  $count = $count + 1;
  $lineCount = $lineCount + 1;
  if ($lineCount == 20) {
  echo rtrim($line), PHP_EOL;
  $line = '';
  $lineCount = 0;
};
}
  $i = $i + 1;
};
  if ($lineCount > 0) {
  echo rtrim($line), PHP_EOL;
}
}
main();
