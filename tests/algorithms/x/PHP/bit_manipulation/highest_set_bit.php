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
function highest_set_bit_position($number) {
  if ($number < 0) {
  $panic('number must be non-negative');
}
  $position = 0;
  $n = $number;
  while ($n > 0) {
  $position = $position + 1;
  $n = _intdiv($n, 2);
};
  return $position;
}
echo rtrim(_str(highest_set_bit_position(25))), PHP_EOL;
echo rtrim(_str(highest_set_bit_position(37))), PHP_EOL;
echo rtrim(_str(highest_set_bit_position(1))), PHP_EOL;
echo rtrim(_str(highest_set_bit_position(4))), PHP_EOL;
echo rtrim(_str(highest_set_bit_position(0))), PHP_EOL;
