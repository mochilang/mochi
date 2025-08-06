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
function binary_count_trailing_zeros($a) {
  if ($a < 0) {
  $panic('Input value must be a non-negative integer');
}
  if ($a == 0) {
  return 0;
}
  $n = $a;
  $count = 0;
  while ($n % 2 == 0) {
  $count = $count + 1;
  $n = _intdiv($n, 2);
};
  return $count;
}
echo rtrim(_str(binary_count_trailing_zeros(25))), PHP_EOL;
echo rtrim(_str(binary_count_trailing_zeros(36))), PHP_EOL;
echo rtrim(_str(binary_count_trailing_zeros(16))), PHP_EOL;
echo rtrim(_str(binary_count_trailing_zeros(58))), PHP_EOL;
echo rtrim(_str(binary_count_trailing_zeros(4294967296))), PHP_EOL;
echo rtrim(_str(binary_count_trailing_zeros(0))), PHP_EOL;
