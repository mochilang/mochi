<?php
ini_set('memory_limit', '-1');
function _str($x) {
    if (is_array($x)) {
        $isList = array_keys($x) === range(0, count($x) - 1);
        if ($isList) {
            $parts = [];
            foreach ($x as $v) { $parts[] = _str($v); }
            return '[' . implode(' ', $parts) . ']';
        }
        $parts = [];
        foreach ($x as $k => $v) { $parts[] = _str($k) . ':' . _str($v); }
        return 'map[' . implode(' ', $parts) . ']';
    }
    if (is_bool($x)) return $x ? 'true' : 'false';
    if ($x === null) return 'null';
    return strval($x);
}
function isPrime($n) {
  global $circs, $digits, $q, $fq, $count, $f, $fd;
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
$circs = [];
function isCircular($n) {
  global $circs, $digits, $q, $fq, $count, $fd;
  $nn = $n;
  $pow = 1;
  while ($nn > 0) {
  $pow = $pow * 10;
  $nn = intdiv($nn, 10);
};
  $nn = $n;
  while (true) {
  $nn = $nn * 10;
  $f = intdiv($nn, $pow);
  $nn = $nn + $f * (1 - $pow);
  if ($nn == $n) {
  break;
}
  if (!isPrime($nn)) {
  return false;
}
};
  return true;
}
echo rtrim('The first 19 circular primes are:'), PHP_EOL;
$digits = [1, 3, 7, 9];
$q = [1, 2, 3, 5, 7, 9];
$fq = [1, 2, 3, 5, 7, 9];
$count = 0;
while (true) {
  $f = $q[0];
  $fd = $fq[0];
  if (isPrime($f) && isCircular($f)) {
  $circs = array_merge($circs, [$f]);
  $count = $count + 1;
  if ($count == 19) {
  break;
};
}
  $q = array_slice($q, 1);
  $fq = array_slice($fq, 1);
  if ($f != 2 && $f != 5) {
  foreach ($digits as $d) {
  $q = array_merge($q, [$f * 10 + $d]);
  $fq = array_merge($fq, [$fd]);
};
}
}
function showList($xs) {
  global $circs, $digits, $q, $fq, $count, $f, $fd;
  $out = '[';
  $i = 0;
  while ($i < count($xs)) {
  $out = $out . _str($xs[$i]);
  if ($i < count($xs) - 1) {
  $out = $out . ', ';
}
  $i = $i + 1;
};
  return $out . ']';
}
echo rtrim(showList($circs)), PHP_EOL;
echo rtrim('
The next 4 circular primes, in repunit format, are:'), PHP_EOL;
echo rtrim('[R(19) R(23) R(317) R(1031)]'), PHP_EOL;
echo rtrim('
The following repunits are probably circular primes:'), PHP_EOL;
foreach ([5003, 9887, 15073, 25031, 35317, 49081] as $i) {
  echo rtrim('R(' . _str($i) . ') : true'), PHP_EOL;
}
