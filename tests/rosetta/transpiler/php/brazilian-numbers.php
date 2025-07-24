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
function sameDigits($n, $b) {
  global $isBrazilian, $isPrime, $main;
  $f = $n % $b;
  $n = intval((intdiv($n, $b)));
  while ($n > 0) {
  if ($n % $b != $f) {
  return false;
}
  $n = intval((intdiv($n, $b)));
};
  return true;
}
function isBrazilian($n) {
  global $sameDigits, $isPrime, $main;
  if ($n < 7) {
  return false;
}
  if ($n % 2 == 0 && $n >= 8) {
  return true;
}
  $b = 2;
  while ($b < $n - 1) {
  if (sameDigits($n, $b)) {
  return true;
}
  $b = $b + 1;
};
  return false;
}
function isPrime($n) {
  global $sameDigits, $isBrazilian, $main;
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
function main() {
  global $sameDigits, $isBrazilian, $isPrime;
  $kinds = [' ', ' odd ', ' prime '];
  foreach ($kinds as $kind) {
  echo rtrim('First 20' . $kind . 'Brazilian numbers:'), PHP_EOL;
  $c = 0;
  $n = 7;
  while (true) {
  if (isBrazilian($n)) {
  echo rtrim(_str($n) . ' '), PHP_EOL;
  $c = $c + 1;
  if ($c == 20) {
  echo rtrim('
'), PHP_EOL;
  break;
};
}
  if ($kind == ' ') {
  $n = $n + 1;
} else {
  if ($kind == ' odd ') {
  $n = $n + 2;
} else {
  while (true) {
  $n = $n + 2;
  if (isPrime($n)) {
  break;
}
};
};
}
};
};
  $n = 7;
  $c = 0;
  while ($c < 100000) {
  if (isBrazilian($n)) {
  $c = $c + 1;
}
  $n = $n + 1;
};
  echo rtrim('The 100,000th Brazilian number: ' . _str($n - 1)), PHP_EOL;
}
main();
