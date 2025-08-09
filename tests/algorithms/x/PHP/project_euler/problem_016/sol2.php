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
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
function solution($power) {
  $digits = [];
  $digits = _append($digits, 1);
  $i = 0;
  while ($i < $power) {
  $carry = 0;
  $j = 0;
  while ($j < count($digits)) {
  $v = $digits[$j] * 2 + $carry;
  $digits[$j] = $v % 10;
  $carry = _intdiv($v, 10);
  $j = $j + 1;
};
  if ($carry > 0) {
  $digits = _append($digits, $carry);
}
  $i = $i + 1;
};
  $sum = 0;
  $k = 0;
  while ($k < count($digits)) {
  $sum = $sum + $digits[$k];
  $k = $k + 1;
};
  return $sum;
}
echo rtrim(_str(solution(1000))), PHP_EOL;
echo rtrim(_str(solution(50))), PHP_EOL;
echo rtrim(_str(solution(20))), PHP_EOL;
echo rtrim(_str(solution(15))), PHP_EOL;
