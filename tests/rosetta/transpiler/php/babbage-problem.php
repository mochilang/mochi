<?php
ini_set('memory_limit', '-1');
$target = 269696;
$modulus = 1000000;
$n = 1;
while (true) {
  $square = $n * $n;
  $ending = $square % $modulus;
  if ($ending == $target) {
  echo rtrim('The smallest number whose square ends with ' . json_encode($target, 1344) . ' is ' . json_encode($n, 1344)), PHP_EOL;
  break;
}
  $n = $n + 1;
}
