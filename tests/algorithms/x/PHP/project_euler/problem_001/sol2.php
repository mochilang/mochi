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
function sum_of_multiples($n) {
  $total = 0;
  $terms = _intdiv(($n - 1), 3);
  $total = $total + _intdiv(($terms * (6 + ($terms - 1) * 3)), 2);
  $terms = _intdiv(($n - 1), 5);
  $total = $total + _intdiv(($terms * (10 + ($terms - 1) * 5)), 2);
  $terms = _intdiv(($n - 1), 15);
  $total = $total - _intdiv(($terms * (30 + ($terms - 1) * 15)), 2);
  return $total;
}
echo rtrim('solution() = ' . _str(sum_of_multiples(1000))), PHP_EOL;
