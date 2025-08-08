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
  if ($num < 0) {
  return false;
}
  $n = $num;
  $rev = 0;
  while ($n > 0) {
  $rev = $rev * 10 + ($n % 10);
  $n = _intdiv($n, 10);
};
  return $rev == $num;
}
function solution($limit) {
  $answer = 0;
  $i = 999;
  while ($i >= 100) {
  $j = 999;
  while ($j >= 100) {
  $product = $i * $j;
  if ($product < $limit && is_palindrome($product) && $product > $answer) {
  $answer = $product;
}
  $j = $j - 1;
};
  $i = $i - 1;
};
  return $answer;
}
echo rtrim(_str(solution(998001))), PHP_EOL;
