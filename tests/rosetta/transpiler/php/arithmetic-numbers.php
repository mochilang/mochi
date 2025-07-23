<?php
function sieve($limit) {
  global $primesFrom, $pad3, $commatize, $primeCount, $arithmeticNumbers, $main;
  $spf = [];
  $i = 0;
  while ($i <= $limit) {
  $spf = array_merge($spf, [0]);
  $i = $i + 1;
};
  $i = 2;
  while ($i <= $limit) {
  if ($spf[$i] == 0) {
  $spf[$i] = $i;
  if ($i * $i <= $limit) {
  $j = $i * $i;
  while ($j <= $limit) {
  if ($spf[$j] == 0) {
  $spf[$j] = $i;
}
  $j = $j + $i;
};
};
}
  $i = $i + 1;
};
  return $spf;
}
function primesFrom(&$spf, $limit) {
  global $sieve, $pad3, $commatize, $primeCount, $arithmeticNumbers, $main;
  $primes = [];
  $i = 3;
  while ($i <= $limit) {
  if ($spf[$i] == $i) {
  $primes = array_merge($primes, [$i]);
}
  $i = $i + 1;
};
  return $primes;
}
function pad3($n) {
  global $sieve, $primesFrom, $commatize, $primeCount, $arithmeticNumbers, $main;
  $s = json_encode($n, 1344);
  while (strlen($s) < 3) {
  $s = " " . $s;
};
  return $s;
}
function commatize($n) {
  global $sieve, $primesFrom, $pad3, $primeCount, $arithmeticNumbers, $main;
  $s = json_encode($n, 1344);
  $out = "";
  $i = strlen($s) - 1;
  $c = 0;
  while ($i >= 0) {
  $out = substr($s, $i, $i + 1 - $i) . $out;
  $c = $c + 1;
  if ($c % 3 == 0 && $i > 0) {
  $out = "," . $out;
}
  $i = $i - 1;
};
  return $out;
}
function primeCount(&$primes, $last, &$spf) {
  global $sieve, $primesFrom, $pad3, $commatize, $arithmeticNumbers, $main;
  $lo = 0;
  $hi = count($primes);
  while ($lo < $hi) {
  $mid = intval((($lo + $hi) / 2));
  if ($primes[$mid] < $last) {
  $lo = $mid + 1;
} else {
  $hi = $mid;
}
};
  $count = $lo + 1;
  if ($spf[$last] != $last) {
  $count = $count - 1;
}
  return $count;
}
function arithmeticNumbers($limit, &$spf) {
  global $sieve, $primesFrom, $pad3, $commatize, $primeCount, $main;
  $arr = [1];
  $n = 3;
  while (count($arr) < $limit) {
  if ($spf[$n] == $n) {
  $arr = array_merge($arr, [$n]);
} else {
  $x = $n;
  $sigma = 1;
  $tau = 1;
  while ($x > 1) {
  $p = $spf[$x];
  if ($p == 0) {
  $p = $x;
}
  $cnt = 0;
  $power = $p;
  $sum = 1;
  while ($x % $p == 0) {
  $x = intdiv($x, $p);
  $cnt = $cnt + 1;
  $sum = $sum + $power;
  $power = $power * $p;
};
  $sigma = $sigma * $sum;
  $tau = $tau * ($cnt + 1);
};
  if ($sigma % $tau == 0) {
  $arr = array_merge($arr, [$n]);
};
}
  $n = $n + 1;
};
  return $arr;
}
function main() {
  global $sieve, $primesFrom, $pad3, $commatize, $primeCount, $arithmeticNumbers;
  $limit = 1228663;
  $spf = sieve($limit);
  $primes = primesFrom($spf, $limit);
  $arr = arithmeticNumbers(1000000, $spf);
  echo "The first 100 arithmetic numbers are:", PHP_EOL;
  $i = 0;
  while ($i < 100) {
  $line = "";
  $j = 0;
  while ($j < 10) {
  $line = $line . pad3($arr[$i + $j]);
  if ($j < 9) {
  $line = $line . " ";
}
  $j = $j + 1;
};
  echo $line, PHP_EOL;
  $i = $i + 10;
};
  foreach ([1000, 10000, 100000, 1000000] as $x) {
  $last = $arr[$x - 1];
  $lastc = commatize($last);
  echo "\nThe " . commatize($x) . "th arithmetic number is: " . $lastc, PHP_EOL;
  $pc = primeCount($primes, $last, $spf);
  $comp = $x - $pc - 1;
  echo "The count of such numbers <= " . $lastc . " which are composite is " . commatize($comp) . ".", PHP_EOL;
};
}
main();
