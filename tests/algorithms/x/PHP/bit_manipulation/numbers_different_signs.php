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
function different_signs($num1, $num2) {
  $sign1 = $num1 < 0;
  $sign2 = $num2 < 0;
  return $sign1 != $sign2;
}
echo rtrim(_str(different_signs(1, -1))), PHP_EOL;
echo rtrim(_str(different_signs(1, 1))), PHP_EOL;
echo rtrim(_str(different_signs(1000000000000000000, -1000000000000000000))), PHP_EOL;
echo rtrim(_str(different_signs(-1000000000000000000, 1000000000000000000))), PHP_EOL;
echo rtrim(_str(different_signs(50, 278))), PHP_EOL;
echo rtrim(_str(different_signs(0, 2))), PHP_EOL;
echo rtrim(_str(different_signs(2, 0))), PHP_EOL;
