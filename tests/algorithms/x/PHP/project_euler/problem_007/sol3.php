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
function is_prime($number) {
  if (1 < $number && $number < 4) {
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
  $count = 0;
  $num = 2;
  while (true) {
  if (is_prime($num)) {
  $count = $count + 1;
  if ($count == $nth) {
  return $num;
};
}
  $num = $num + 1;
};
  return 0;
}
echo rtrim('solution() = ' . _str(solution(10001))), PHP_EOL;
