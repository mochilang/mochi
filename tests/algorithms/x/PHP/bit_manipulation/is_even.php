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
function is_even($number) {
  return $number % 2 == 0;
}
echo rtrim(_str(is_even(1))), PHP_EOL;
echo rtrim(_str(is_even(4))), PHP_EOL;
echo rtrim(_str(is_even(9))), PHP_EOL;
echo rtrim(_str(is_even(15))), PHP_EOL;
echo rtrim(_str(is_even(40))), PHP_EOL;
echo rtrim(_str(is_even(100))), PHP_EOL;
echo rtrim(_str(is_even(101))), PHP_EOL;
