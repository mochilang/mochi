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
function int_to_binary($n) {
  if ($n == 0) {
  return '0';
}
  $res = '';
  $num = $n;
  while ($num > 0) {
  $res = _str($num % 2) . $res;
  $num = _intdiv($num, 2);
};
  return $res;
}
function pad_left($s, $width) {
  $res = $s;
  while (strlen($res) < $width) {
  $res = '0' . $res;
};
  return $res;
}
function binary_xor($a, $b) {
  if ($a < 0 || $b < 0) {
  $panic('the value of both inputs must be positive');
}
  $a_bin = int_to_binary($a);
  $b_bin = int_to_binary($b);
  $max_len = (strlen($a_bin) > strlen($b_bin) ? strlen($a_bin) : strlen($b_bin));
  $a_pad = pad_left($a_bin, $max_len);
  $b_pad = pad_left($b_bin, $max_len);
  $i = 0;
  $result = '';
  while ($i < $max_len) {
  if (substr($a_pad, $i, $i + 1 - $i) != substr($b_pad, $i, $i + 1 - $i)) {
  $result = $result . '1';
} else {
  $result = $result . '0';
}
  $i = $i + 1;
};
  return '0b' . $result;
}
echo rtrim(binary_xor(25, 32)), PHP_EOL;
echo rtrim(binary_xor(37, 50)), PHP_EOL;
echo rtrim(binary_xor(21, 30)), PHP_EOL;
echo rtrim(binary_xor(58, 73)), PHP_EOL;
echo rtrim(binary_xor(0, 255)), PHP_EOL;
echo rtrim(binary_xor(256, 256)), PHP_EOL;
