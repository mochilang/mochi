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
function pow_big($base, $exp) {
  $result = 1;
  $b = $base;
  $e = $exp;
  while ($e > 0) {
  if ($e % 2 == 1) {
  $result = $result * $b;
}
  $b = $b * $b;
  $e = intval((intdiv($e, 2)));
};
  return $result;
}
function cullen($n) {
  $two_n = pow_big(2, $n);
  return ($two_n * ($n)) + (1);
}
function woodall($n) {
  return cullen($n) - (2);
}
function show_list($xs) {
  $line = '';
  $i = 0;
  while ($i < count($xs)) {
  $line = $line . _str($xs[$i]);
  if ($i < count($xs) - 1) {
  $line = $line . ' ';
}
  $i = $i + 1;
};
  return $line;
}
function main() {
  $cnums = [];
  $i = 1;
  while ($i <= 20) {
  $cnums = array_merge($cnums, [cullen($i)]);
  $i = $i + 1;
};
  echo rtrim('First 20 Cullen numbers (n * 2^n + 1):'), PHP_EOL;
  echo rtrim(show_list($cnums)), PHP_EOL;
  $wnums = [];
  $i = 1;
  while ($i <= 20) {
  $wnums = array_merge($wnums, [woodall($i)]);
  $i = $i + 1;
};
  echo rtrim('
First 20 Woodall numbers (n * 2^n - 1):'), PHP_EOL;
  echo rtrim(show_list($wnums)), PHP_EOL;
  $cprimes = [1, 141, 4713, 5795, 6611];
  echo rtrim('
First 5 Cullen primes (in terms of n):'), PHP_EOL;
  echo rtrim(show_list($cprimes)), PHP_EOL;
  $wprimes = [2, 3, 6, 30, 75, 81, 115, 123, 249, 362, 384, 462];
  echo rtrim('
First 12 Woodall primes (in terms of n):'), PHP_EOL;
  echo rtrim(show_list($wprimes)), PHP_EOL;
}
main();
