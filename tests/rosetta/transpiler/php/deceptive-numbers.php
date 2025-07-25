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
function listToString($xs) {
  $s = '[';
  $i = 0;
  while ($i < count($xs)) {
  $s = $s . _str($xs[$i]);
  if ($i < count($xs) - 1) {
  $s = $s . ' ';
}
  $i = $i + 1;
};
  return $s . ']';
}
function main() {
  $count = 0;
  $limit = 25;
  $n = 17;
  $repunit = 1111111111111111;
  $eleven = 11;
  $hundred = 100;
  $deceptive = [];
  while ($count < $limit) {
  if (!isPrime($n) && $n % 3 != 0 && $n % 5 != 0) {
  $bn = $n;
  if ($repunit % $bn == 0) {
  $deceptive = array_merge($deceptive, [$n]);
  $count = $count + 1;
};
}
  $n = $n + 2;
  $repunit = ($repunit * $hundred) + $eleven;
};
  echo rtrim('The first ' . _str($limit) . ' deceptive numbers are:'), PHP_EOL;
  echo rtrim(listToString($deceptive)), PHP_EOL;
}
main();
