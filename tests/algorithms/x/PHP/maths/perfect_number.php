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
function perfect($n) {
  if ($n <= 0) {
  return false;
}
  $total = 0;
  $divisor = 1;
  while ($divisor <= _intdiv($n, 2)) {
  if ($n % $divisor == 0) {
  $total = $total + $divisor;
}
  $divisor = $divisor + 1;
};
  return $total == $n;
}
echo rtrim(_str(perfect(27))), PHP_EOL;
echo rtrim(_str(perfect(28))), PHP_EOL;
echo rtrim(_str(perfect(29))), PHP_EOL;
echo rtrim(_str(perfect(6))), PHP_EOL;
echo rtrim(_str(perfect(12))), PHP_EOL;
echo rtrim(_str(perfect(496))), PHP_EOL;
echo rtrim(_str(perfect(8128))), PHP_EOL;
echo rtrim(_str(perfect(0))), PHP_EOL;
echo rtrim(_str(perfect(-1))), PHP_EOL;
echo rtrim(_str(perfect(33550336))), PHP_EOL;
echo rtrim(_str(perfect(33550337))), PHP_EOL;
echo rtrim(_str(perfect(1))), PHP_EOL;
