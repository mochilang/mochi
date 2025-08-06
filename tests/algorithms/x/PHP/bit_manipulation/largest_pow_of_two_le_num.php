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
function largest_pow_of_two_le_num($n) {
  if ($n <= 0) {
  return 0;
}
  $res = 1;
  while ($res * 2 <= $n) {
  $res = $res * 2;
};
  return $res;
}
echo rtrim(_str(largest_pow_of_two_le_num(0))), PHP_EOL;
echo rtrim(_str(largest_pow_of_two_le_num(1))), PHP_EOL;
echo rtrim(_str(largest_pow_of_two_le_num(-1))), PHP_EOL;
echo rtrim(_str(largest_pow_of_two_le_num(3))), PHP_EOL;
echo rtrim(_str(largest_pow_of_two_le_num(15))), PHP_EOL;
echo rtrim(_str(largest_pow_of_two_le_num(99))), PHP_EOL;
echo rtrim(_str(largest_pow_of_two_le_num(178))), PHP_EOL;
echo rtrim(_str(largest_pow_of_two_le_num(999999))), PHP_EOL;
