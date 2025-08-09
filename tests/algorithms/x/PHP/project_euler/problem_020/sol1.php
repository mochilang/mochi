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
function factorial($num) {
  $res = 1;
  $i = 2;
  while ($i <= $num) {
  $res = $res * $i;
  $i = $i + 1;
};
  return $res;
}
function split_and_add($number) {
  $sum = 0;
  $n = $number;
  while ($n > 0) {
  $last = $n % 10;
  $sum = $sum + $last;
  $n = _intdiv($n, 10);
};
  return $sum;
}
function solution($num) {
  $nfact = factorial($num);
  return split_and_add($nfact);
}
echo rtrim(_str(solution(100))), PHP_EOL;
