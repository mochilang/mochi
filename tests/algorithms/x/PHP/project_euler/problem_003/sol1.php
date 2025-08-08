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
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
function is_prime($number) {
  if ($number > 1 && $number < 4) {
  return true;
}
  if ($number < 2 || $number % 2 == 0 || $number % 3 == 0) {
  return false;
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
function solution($n) {
  $num = $n;
  if ($num <= 0) {
  echo rtrim('Parameter n must be greater than or equal to one.'), PHP_EOL;
  return 0;
}
  if (is_prime($num)) {
  return $num;
}
  while ($num % 2 == 0) {
  $num = _intdiv($num, 2);
  if (is_prime($num)) {
  return $num;
}
};
  $max_number = 1;
  $i = 3;
  while ($i * $i <= $num) {
  if ($num % $i == 0) {
  if (is_prime(_intdiv($num, $i))) {
  $max_number = _intdiv($num, $i);
  break;
} else {
  if (is_prime($i)) {
  $max_number = $i;
};
};
}
  $i = $i + 2;
};
  return $max_number;
}
function main() {
  $result = solution(600851475143);
  echo rtrim('solution() = ' . _str($result)), PHP_EOL;
}
main();
