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
function pow2($exp) {
  $result = 1;
  $i = 0;
  while ($i < $exp) {
  $result = $result * 2;
  $i = $i + 1;
};
  return $result;
}
function is_bit_set($number, $position) {
  $shifted = $number / pow2($position);
  $remainder = $shifted % 2;
  return $remainder == 1;
}
function set_bit($number, $position) {
  if (is_bit_set($number, $position)) {
  return $number;
}
  return $number + pow2($position);
}
function clear_bit($number, $position) {
  if (is_bit_set($number, $position)) {
  return $number - pow2($position);
}
  return $number;
}
function flip_bit($number, $position) {
  if (is_bit_set($number, $position)) {
  return $number - pow2($position);
}
  return $number + pow2($position);
}
function get_bit($number, $position) {
  if (is_bit_set($number, $position)) {
  return 1;
}
  return 0;
}
echo rtrim(_str(set_bit(13, 1))), PHP_EOL;
echo rtrim(_str(clear_bit(18, 1))), PHP_EOL;
echo rtrim(_str(flip_bit(5, 1))), PHP_EOL;
echo rtrim(_str(is_bit_set(10, 3))), PHP_EOL;
echo rtrim(_str(get_bit(10, 1))), PHP_EOL;
