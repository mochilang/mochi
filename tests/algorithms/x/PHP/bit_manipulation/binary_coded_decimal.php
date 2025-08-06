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
function to_binary4($n) {
  $result = '';
  $x = $n;
  while ($x > 0) {
  $result = _str($x % 2) . $result;
  $x = _intdiv($x, 2);
};
  while (strlen($result) < 4) {
  $result = '0' . $result;
};
  return $result;
}
function binary_coded_decimal($number) {
  $n = $number;
  if ($n < 0) {
  $n = 0;
}
  $digits = _str($n);
  $out = '0b';
  $i = 0;
  while ($i < strlen($digits)) {
  $d = substr($digits, $i, $i + 1 - $i);
  $d_int = intval($d);
  $out = $out . to_binary4($d_int);
  $i = $i + 1;
};
  return $out;
}
echo rtrim(binary_coded_decimal(-2)), PHP_EOL;
echo rtrim(binary_coded_decimal(-1)), PHP_EOL;
echo rtrim(binary_coded_decimal(0)), PHP_EOL;
echo rtrim(binary_coded_decimal(3)), PHP_EOL;
echo rtrim(binary_coded_decimal(2)), PHP_EOL;
echo rtrim(binary_coded_decimal(12)), PHP_EOL;
echo rtrim(binary_coded_decimal(987)), PHP_EOL;
