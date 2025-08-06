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
function to_binary($n) {
  if ($n == 0) {
  return '0';
}
  $num = $n;
  $res = '';
  while ($num > 0) {
  $bit = $num % 2;
  $res = _str($bit) . $res;
  $num = _intdiv($num, 2);
};
  return $res;
}
function zfill($s, $width) {
  $res = $s;
  $pad = $width - strlen($s);
  while ($pad > 0) {
  $res = '0' . $res;
  $pad = $pad - 1;
};
  return $res;
}
function binary_and($a, $b) {
  if ($a < 0 || $b < 0) {
  $panic('the value of both inputs must be positive');
}
  $a_bin = to_binary($a);
  $b_bin = to_binary($b);
  $max_len = strlen($a_bin);
  if (strlen($b_bin) > $max_len) {
  $max_len = strlen($b_bin);
}
  $a_pad = zfill($a_bin, $max_len);
  $b_pad = zfill($b_bin, $max_len);
  $i = 0;
  $res = '';
  while ($i < $max_len) {
  if (substr($a_pad, $i, $i + 1 - $i) == '1' && substr($b_pad, $i, $i + 1 - $i) == '1') {
  $res = $res . '1';
} else {
  $res = $res . '0';
}
  $i = $i + 1;
};
  return '0b' . $res;
}
echo rtrim(binary_and(25, 32)), PHP_EOL;
echo rtrim(binary_and(37, 50)), PHP_EOL;
echo rtrim(binary_and(21, 30)), PHP_EOL;
echo rtrim(binary_and(58, 73)), PHP_EOL;
echo rtrim(binary_and(0, 255)), PHP_EOL;
echo rtrim(binary_and(256, 256)), PHP_EOL;
