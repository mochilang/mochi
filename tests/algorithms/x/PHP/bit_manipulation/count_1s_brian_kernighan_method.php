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
function lowest_set_bit($n) {
  $lb = 1;
  while ($n % ($lb * 2) == 0) {
  $lb = $lb * 2;
};
  return $lb;
}
function get_1s_count($number) {
  if ($number < 0) {
  echo rtrim('ValueError: Input must be a non-negative integer'), PHP_EOL;
  return 0;
}
  $n = $number;
  $count = 0;
  while ($n > 0) {
  $n = $n - lowest_set_bit($n);
  $count = $count + 1;
};
  return $count;
}
echo rtrim(_str(get_1s_count(25))), PHP_EOL;
echo rtrim(_str(get_1s_count(37))), PHP_EOL;
echo rtrim(_str(get_1s_count(21))), PHP_EOL;
echo rtrim(_str(get_1s_count(58))), PHP_EOL;
echo rtrim(_str(get_1s_count(0))), PHP_EOL;
echo rtrim(_str(get_1s_count(256))), PHP_EOL;
