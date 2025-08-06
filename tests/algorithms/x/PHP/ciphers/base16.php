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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
function base16_encode($data) {
  global $example1, $example2;
  $digits = '0123456789ABCDEF';
  $res = '';
  $i = 0;
  while ($i < count($data)) {
  $b = $data[$i];
  if ($b < 0 || $b > 255) {
  $panic('byte out of range');
}
  $hi = _intdiv($b, 16);
  $lo = $b % 16;
  $res = $res . substr($digits, $hi, $hi + 1 - $hi) . substr($digits, $lo, $lo + 1 - $lo);
  $i = $i + 1;
};
  return $res;
}
function base16_decode($data) {
  global $example1, $example2;
  $digits = '0123456789ABCDEF';
  if (fmod(strlen($data), 2) != 0) {
  $panic('Base16 encoded data is invalid: Data does not have an even number of hex digits.');
}
  $hex_value = null;
$hex_value = function($ch) use (&$hex_value, $data, $digits) {
  $j = 0;
  while ($j < 16) {
  if (substr($digits, $j, $j + 1 - $j) == $ch) {
  return $j;
}
  $j = $j + 1;
};
  return -1;
};
  $out = [];
  $i = 0;
  while ($i < strlen($data)) {
  $hi_char = substr($data, $i, $i + 1 - $i);
  $lo_char = substr($data, $i + 1, $i + 2 - ($i + 1));
  $hi = $hex_value($hi_char);
  $lo = $hex_value($lo_char);
  if ($hi < 0 || $lo < 0) {
  $panic('Base16 encoded data is invalid: Data is not uppercase hex or it contains invalid characters.');
}
  $out = _append($out, $hi * 16 + $lo);
  $i = $i + 2;
};
  return $out;
}
$example1 = [72, 101, 108, 108, 111, 32, 87, 111, 114, 108, 100, 33];
$example2 = [72, 69, 76, 76, 79, 32, 87, 79, 82, 76, 68, 33];
echo rtrim(base16_encode($example1)), PHP_EOL;
echo rtrim(base16_encode($example2)), PHP_EOL;
echo rtrim(base16_encode([])), PHP_EOL;
echo rtrim(_str(base16_decode('48656C6C6F20576F726C6421'))), PHP_EOL;
echo rtrim(_str(base16_decode('48454C4C4F20574F524C4421'))), PHP_EOL;
echo rtrim(_str(base16_decode(''))), PHP_EOL;
