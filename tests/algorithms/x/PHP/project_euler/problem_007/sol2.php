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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
function is_prime($number) {
  global $ans;
  if ($number > 1 && $number < 4) {
  return true;
} else {
  if ($number < 2 || $number % 2 == 0 || $number % 3 == 0) {
  return false;
};
}
  $i = 5;
  while ($i * $i <= $number) {
  if ($number % $i == 0 || $number % ($i + 2) == 0) {
  return false;
}
  $i = $i + 6;
};
  return true;
}
function solution($nth) {
  global $ans;
  if ($nth <= 0) {
  _panic('Parameter nth must be greater than or equal to one.');
}
  $primes = [];
  $num = 2;
  while (count($primes) < $nth) {
  if (is_prime($num)) {
  $primes = _append($primes, $num);
}
  $num = $num + 1;
};
  return $primes[count($primes) - 1];
}
$ans = solution(10001);
echo rtrim('solution(10001) = ' . _str($ans)), PHP_EOL;
