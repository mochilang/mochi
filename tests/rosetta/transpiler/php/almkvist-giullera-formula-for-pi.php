<?php
function bigTrim(&$a) {
  global $bigFromInt, $bigCmp, $bigAdd, $bigSub, $bigMulSmall, $bigMulBig, $bigMulPow10, $bigDivSmall, $bigToString, $repeat, $sortInts, $primesUpTo, $factorialExp, $factorSmall, $computeIP, $formatTerm, $bigAbsDiff, $main;
  $n = count($a);
  while ($n > 1 && $a[$n - 1] == 0) {
  $a = array_slice($a, 0, $n - 1 - 0);
  $n = $n - 1;
};
  return $a;
}
function bigFromInt($x) {
  global $bigTrim, $bigCmp, $bigAdd, $bigSub, $bigMulSmall, $bigMulBig, $bigMulPow10, $bigDivSmall, $bigToString, $repeat, $sortInts, $primesUpTo, $factorialExp, $factorSmall, $computeIP, $formatTerm, $bigAbsDiff, $main;
  if ($x == 0) {
  return [0];
}
  $digits = [];
  $n = $x;
  while ($n > 0) {
  $digits = array_merge($digits, [$n % 10]);
  $n = intdiv($n, 10);
};
  return $digits;
}
function bigCmp(&$a, &$b) {
  global $bigTrim, $bigFromInt, $bigAdd, $bigSub, $bigMulSmall, $bigMulBig, $bigMulPow10, $bigDivSmall, $bigToString, $repeat, $sortInts, $primesUpTo, $factorialExp, $factorSmall, $computeIP, $formatTerm, $bigAbsDiff, $main;
  if (count($a) > count($b)) {
  return 1;
}
  if (count($a) < count($b)) {
  return -1;
}
  $i = count($a) - 1;
  while ($i >= 0) {
  if ($a[$i] > $b[$i]) {
  return 1;
}
  if ($a[$i] < $b[$i]) {
  return -1;
}
  $i = $i - 1;
};
  return 0;
}
function bigAdd(&$a, &$b) {
  global $bigTrim, $bigFromInt, $bigCmp, $bigSub, $bigMulSmall, $bigMulBig, $bigMulPow10, $bigDivSmall, $bigToString, $repeat, $sortInts, $primesUpTo, $factorialExp, $factorSmall, $computeIP, $formatTerm, $bigAbsDiff, $main;
  $res = [];
  $carry = 0;
  $i = 0;
  while ($i < count($a) || $i < count($b) || $carry > 0) {
  $av = 0;
  if ($i < count($a)) {
  $av = $a[$i];
}
  $bv = 0;
  if ($i < count($b)) {
  $bv = $b[$i];
}
  $s = $av + $bv + $carry;
  $res = array_merge($res, [$s % 10]);
  $carry = intdiv($s, 10);
  $i = $i + 1;
};
  return bigTrim($res);
}
function bigSub(&$a, &$b) {
  global $bigTrim, $bigFromInt, $bigCmp, $bigAdd, $bigMulSmall, $bigMulBig, $bigMulPow10, $bigDivSmall, $bigToString, $repeat, $sortInts, $primesUpTo, $factorialExp, $factorSmall, $computeIP, $formatTerm, $bigAbsDiff, $main;
  $res = [];
  $borrow = 0;
  $i = 0;
  while ($i < count($a)) {
  $av = $a[$i];
  $bv = 0;
  if ($i < count($b)) {
  $bv = $b[$i];
}
  $diff = $av - $bv - $borrow;
  if ($diff < 0) {
  $diff = $diff + 10;
  $borrow = 1;
} else {
  $borrow = 0;
}
  $res = array_merge($res, [$diff]);
  $i = $i + 1;
};
  return bigTrim($res);
}
function bigMulSmall(&$a, $m) {
  global $bigTrim, $bigFromInt, $bigCmp, $bigAdd, $bigSub, $bigMulBig, $bigMulPow10, $bigDivSmall, $bigToString, $repeat, $sortInts, $primesUpTo, $factorialExp, $factorSmall, $computeIP, $formatTerm, $bigAbsDiff, $main;
  if ($m == 0) {
  return [0];
}
  $res = [];
  $carry = 0;
  $i = 0;
  while ($i < count($a)) {
  $prod = $a[$i] * $m + $carry;
  $res = array_merge($res, [$prod % 10]);
  $carry = intdiv($prod, 10);
  $i = $i + 1;
};
  while ($carry > 0) {
  $res = array_merge($res, [$carry % 10]);
  $carry = intdiv($carry, 10);
};
  return bigTrim($res);
}
function bigMulBig(&$a, &$b) {
  global $bigTrim, $bigFromInt, $bigCmp, $bigAdd, $bigSub, $bigMulSmall, $bigMulPow10, $bigDivSmall, $bigToString, $repeat, $sortInts, $primesUpTo, $factorialExp, $factorSmall, $computeIP, $formatTerm, $bigAbsDiff, $main;
  $res = [];
  $i = 0;
  while ($i < count($a) + count($b)) {
  $res = array_merge($res, [0]);
  $i = $i + 1;
};
  $i = 0;
  while ($i < count($a)) {
  $carry = 0;
  $j = 0;
  while ($j < count($b)) {
  $idx = $i + $j;
  $prod = $res[$idx] + $a[$i] * $b[$j] + $carry;
  $res[$idx] = $prod % 10;
  $carry = intdiv($prod, 10);
  $j = $j + 1;
};
  $idx = $i + count($b);
  while ($carry > 0) {
  $prod = $res[$idx] + $carry;
  $res[$idx] = $prod % 10;
  $carry = intdiv($prod, 10);
  $idx = $idx + 1;
};
  $i = $i + 1;
};
  return bigTrim($res);
}
function bigMulPow10(&$a, $k) {
  global $bigTrim, $bigFromInt, $bigCmp, $bigAdd, $bigSub, $bigMulSmall, $bigMulBig, $bigDivSmall, $bigToString, $repeat, $sortInts, $primesUpTo, $factorialExp, $factorSmall, $computeIP, $formatTerm, $bigAbsDiff, $main;
  $i = 0;
  while ($i < $k) {
  $a = array_merge([0], $a);
  $i = $i + 1;
};
  return $a;
}
function bigDivSmall(&$a, $m) {
  global $bigTrim, $bigFromInt, $bigCmp, $bigAdd, $bigSub, $bigMulSmall, $bigMulBig, $bigMulPow10, $bigToString, $repeat, $sortInts, $primesUpTo, $factorialExp, $factorSmall, $computeIP, $formatTerm, $bigAbsDiff, $main;
  $res = [];
  $rem = 0;
  $i = count($a) - 1;
  while ($i >= 0) {
  $cur = $rem * 10 + $a[$i];
  $q = intdiv($cur, $m);
  $rem = $cur % $m;
  $res = array_merge([$q], $res);
  $i = $i - 1;
};
  return bigTrim($res);
}
function bigToString(&$a) {
  global $bigTrim, $bigFromInt, $bigCmp, $bigAdd, $bigSub, $bigMulSmall, $bigMulBig, $bigMulPow10, $bigDivSmall, $repeat, $sortInts, $primesUpTo, $factorialExp, $factorSmall, $computeIP, $formatTerm, $bigAbsDiff, $main;
  $s = "";
  $i = count($a) - 1;
  while ($i >= 0) {
  $s = $s . json_encode($a[$i], 1344);
  $i = $i - 1;
};
  return $s;
}
function repeat($ch, $n) {
  global $bigTrim, $bigFromInt, $bigCmp, $bigAdd, $bigSub, $bigMulSmall, $bigMulBig, $bigMulPow10, $bigDivSmall, $bigToString, $sortInts, $primesUpTo, $factorialExp, $factorSmall, $computeIP, $formatTerm, $bigAbsDiff, $main;
  $s = "";
  $i = 0;
  while ($i < $n) {
  $s = $s . $ch;
  $i = $i + 1;
};
  return $s;
}
function sortInts(&$xs) {
  global $bigTrim, $bigFromInt, $bigCmp, $bigAdd, $bigSub, $bigMulSmall, $bigMulBig, $bigMulPow10, $bigDivSmall, $bigToString, $repeat, $primesUpTo, $factorialExp, $factorSmall, $computeIP, $formatTerm, $bigAbsDiff, $main;
  $res = [];
  $tmp = $xs;
  while (count($tmp) > 0) {
  $min = $tmp[0];
  $idx = 0;
  $i = 1;
  while ($i < count($tmp)) {
  if ($tmp[$i] < $min) {
  $min = $tmp[$i];
  $idx = $i;
}
  $i = $i + 1;
};
  $res = array_merge($res, [$min]);
  $out = [];
  $j = 0;
  while ($j < count($tmp)) {
  if ($j != $idx) {
  $out = array_merge($out, [$tmp[$j]]);
}
  $j = $j + 1;
};
  $tmp = $out;
};
  return $res;
}
function primesUpTo($n) {
  global $bigTrim, $bigFromInt, $bigCmp, $bigAdd, $bigSub, $bigMulSmall, $bigMulBig, $bigMulPow10, $bigDivSmall, $bigToString, $repeat, $sortInts, $factorialExp, $factorSmall, $computeIP, $formatTerm, $bigAbsDiff, $main;
  $sieve = [];
  $i = 0;
  while ($i <= $n) {
  $sieve = array_merge($sieve, [true]);
  $i = $i + 1;
};
  $p = 2;
  while ($p * $p <= $n) {
  if ($sieve[$p]) {
  $m = $p * $p;
  while ($m <= $n) {
  $sieve[$m] = false;
  $m = $m + $p;
};
}
  $p = $p + 1;
};
  $res = [];
  $x = 2;
  while ($x <= $n) {
  if ($sieve[$x]) {
  $res = array_merge($res, [$x]);
}
  $x = $x + 1;
};
  return $res;
}
function factorialExp($n, &$primes) {
  global $bigTrim, $bigFromInt, $bigCmp, $bigAdd, $bigSub, $bigMulSmall, $bigMulBig, $bigMulPow10, $bigDivSmall, $bigToString, $repeat, $sortInts, $primesUpTo, $factorSmall, $computeIP, $formatTerm, $bigAbsDiff, $main;
  $m = [];
  foreach ($primes as $p) {
  if ($p > $n) {
  break;
}
  $t = $n;
  $e = 0;
  while ($t > 0) {
  $t = intdiv($t, $p);
  $e = $e + $t;
};
  $m[json_encode($p, 1344)] = $e;
};
  return $m;
}
function factorSmall($x, &$primes) {
  global $bigTrim, $bigFromInt, $bigCmp, $bigAdd, $bigSub, $bigMulSmall, $bigMulBig, $bigMulPow10, $bigDivSmall, $bigToString, $repeat, $sortInts, $primesUpTo, $factorialExp, $computeIP, $formatTerm, $bigAbsDiff, $main;
  $f = [];
  $n = $x;
  foreach ($primes as $p) {
  if ($p * $p > $n) {
  break;
}
  $c = 0;
  while ($n % $p == 0) {
  $c = $c + 1;
  $n = intdiv($n, $p);
};
  if ($c > 0) {
  $f[json_encode($p, 1344)] = $c;
}
};
  if ($n > 1) {
  $f[json_encode($n, 1344)] = (array_key_exists(json_encode($n, 1344), $f) ? $f[json_encode($n, 1344)] : 0) + 1;
}
  return $f;
}
function computeIP($n, &$primes) {
  global $bigTrim, $bigFromInt, $bigCmp, $bigAdd, $bigSub, $bigMulSmall, $bigMulBig, $bigMulPow10, $bigDivSmall, $bigToString, $repeat, $sortInts, $primesUpTo, $factorialExp, $factorSmall, $formatTerm, $bigAbsDiff, $main;
  $exps = factorialExp(6 * $n, $primes);
  $fn = factorialExp($n, $primes);
  foreach (array_keys($fn) as $k) {
  $exps[$k] = (array_key_exists($k, $exps) ? $exps[$k] : 0) - 6 * $fn[$k];
};
  $exps["2"] = (array_key_exists("2", $exps) ? $exps["2"] : 0) + 5;
  $t2 = 532 * $n * $n + 126 * $n + 9;
  $ft2 = factorSmall($t2, $primes);
  foreach (array_keys($ft2) as $k) {
  $exps[$k] = (array_key_exists($k, $exps) ? $exps[$k] : 0) + $ft2[$k];
};
  $exps["3"] = (array_key_exists("3", $exps) ? $exps["3"] : 0) - 1;
  $keys = [];
  foreach (array_keys($exps) as $k) {
  $keys = array_merge($keys, [intval($k)]);
};
  $keys = sortInts($keys);
  $res = bigFromInt(1);
  foreach ($keys as $p) {
  $e = $exps[json_encode($p, 1344)];
  $i = 0;
  while ($i < $e) {
  $res = bigMulSmall($res, $p);
  $i = $i + 1;
};
};
  return $res;
}
function formatTerm(&$ip, $pw) {
  global $bigTrim, $bigFromInt, $bigCmp, $bigAdd, $bigSub, $bigMulSmall, $bigMulBig, $bigMulPow10, $bigDivSmall, $bigToString, $repeat, $sortInts, $primesUpTo, $factorialExp, $factorSmall, $computeIP, $bigAbsDiff, $main;
  $s = bigToString($ip);
  if ($pw >= strlen($s)) {
  $frac = repeat("0", $pw - strlen($s)) . $s;
  if (strlen($frac) < 33) {
  $frac = $frac . repeat("0", 33 - strlen($frac));
};
  return "0." . substr($frac, 0, 33 - 0);
}
  $intpart = substr($s, 0, strlen($s) - $pw - 0);
  $frac = substr($s, strlen($s) - $pw, strlen($s) - strlen($s) - $pw);
  if (strlen($frac) < 33) {
  $frac = $frac . repeat("0", 33 - strlen($frac));
}
  return $intpart . "." . substr($frac, 0, 33 - 0);
}
function bigAbsDiff(&$a, &$b) {
  global $bigTrim, $bigFromInt, $bigCmp, $bigAdd, $bigSub, $bigMulSmall, $bigMulBig, $bigMulPow10, $bigDivSmall, $bigToString, $repeat, $sortInts, $primesUpTo, $factorialExp, $factorSmall, $computeIP, $formatTerm, $main;
  if (bigCmp($a, $b) >= 0) {
  return bigSub($a, $b);
}
  return bigSub($b, $a);
}
function main() {
  global $bigTrim, $bigFromInt, $bigCmp, $bigAdd, $bigSub, $bigMulSmall, $bigMulBig, $bigMulPow10, $bigDivSmall, $bigToString, $repeat, $sortInts, $primesUpTo, $factorialExp, $factorSmall, $computeIP, $formatTerm, $bigAbsDiff;
  $primes = primesUpTo(2000);
  echo "N                               Integer Portion  Pow  Nth Term (33 dp)", PHP_EOL;
  $line = repeat("-", 89);
  echo $line, PHP_EOL;
  $sum = bigFromInt(0);
  $prev = bigFromInt(0);
  $denomPow = 0;
  $n = 0;
  while (true) {
  $ip = computeIP($n, $primes);
  $pw = 6 * $n + 3;
  if ($pw > $denomPow) {
  $sum = bigMulPow10($sum, $pw - $denomPow);
  $prev = bigMulPow10($prev, $pw - $denomPow);
  $denomPow = $pw;
}
  if ($n < 10) {
  $termStr = formatTerm($ip, $pw);
  $ipStr = bigToString($ip);
  while (strlen($ipStr) < 44) {
  $ipStr = " " . $ipStr;
};
  $pwStr = json_encode(-$pw, 1344);
  while (strlen($pwStr) < 3) {
  $pwStr = " " . $pwStr;
};
  $padTerm = $termStr;
  while (strlen($padTerm) < 35) {
  $padTerm = $padTerm . " ";
};
  echo json_encode($n, 1344) . "  " . $ipStr . "  " . $pwStr . "  " . $padTerm, PHP_EOL;
}
  $sum = bigAdd($sum, $ip);
  $diff = bigAbsDiff($sum, $prev);
  if ($denomPow >= 70 && bigCmp($diff, bigMulPow10(bigFromInt(1), $denomPow - 70)) < 0) {
  break;
}
  $prev = $sum;
  $n = $n + 1;
};
  $precision = 70;
  $target = bigMulPow10(bigFromInt(1), $denomPow + 2 * $precision);
  $low = bigFromInt(0);
  $high = bigMulPow10(bigFromInt(1), $precision + 1);
  while (bigCmp($low, bigSub($high, bigFromInt(1))) < 0) {
  $mid = bigDivSmall(bigAdd($low, $high), 2);
  $prod = bigMulBig(bigMulBig($mid, $mid), $sum);
  if (bigCmp($prod, $target) <= 0) {
  $low = $mid;
} else {
  $high = bigSub($mid, bigFromInt(1));
}
};
  $piInt = $low;
  $piStr = bigToString($piInt);
  if (strlen($piStr) <= $precision) {
  $piStr = repeat("0", $precision - strlen($piStr) + 1) . $piStr;
}
  $out = substr($piStr, 0, strlen($piStr) - $precision - 0) . "." . substr($piStr, strlen($piStr) - $precision, strlen($piStr) - strlen($piStr) - $precision);
  echo "", PHP_EOL;
  echo "Pi to 70 decimal places is:", PHP_EOL;
  echo $out, PHP_EOL;
}
main();
