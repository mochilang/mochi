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
function is_palindrome($num) {
  $s = _str($num);
  $i = 0;
  $j = strlen($s) - 1;
  while ($i < $j) {
  if (substr($s, $i, $i + 1 - $i) != substr($s, $j, $j + 1 - $j)) {
  return false;
}
  $i = $i + 1;
  $j = $j - 1;
};
  return true;
}
function solution($n) {
  $number = $n - 1;
  while ($number > 9999) {
  if (is_palindrome($number)) {
  $divisor = 999;
  while ($divisor > 99) {
  if ($number % $divisor == 0) {
  $other = _intdiv($number, $divisor);
  if (strlen(_str($other)) == 3) {
  return $number;
};
}
  $divisor = $divisor - 1;
};
}
  $number = $number - 1;
};
  echo rtrim('That number is larger than our acceptable range.'), PHP_EOL;
  return 0;
}
echo rtrim('solution() = ' . _str(solution(998001))), PHP_EOL;
