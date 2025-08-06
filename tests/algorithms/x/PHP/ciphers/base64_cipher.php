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
function repeat($s, $n) {
    return str_repeat($s, intval($n));
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
$B64_CHARSET = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
function to_binary($n) {
  global $B64_CHARSET;
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
  global $B64_CHARSET;
  $res = $s;
  $pad = $width - strlen($s);
  while ($pad > 0) {
  $res = '0' . $res;
  $pad = $pad - 1;
};
  return $res;
}
function from_binary($s) {
  global $B64_CHARSET;
  $i = 0;
  $result = 0;
  while ($i < strlen($s)) {
  $result = $result * 2;
  if (substr($s, $i, $i + 1 - $i) == '1') {
  $result = $result + 1;
}
  $i = $i + 1;
};
  return $result;
}
function mochi_repeat($ch, $times) {
  global $B64_CHARSET;
  $res = '';
  $i = 0;
  while ($i < $times) {
  $res = $res . $ch;
  $i = $i + 1;
};
  return $res;
}
function char_index($s, $c) {
  global $B64_CHARSET;
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $c) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
}
function mochi_base64_encode($data) {
  global $B64_CHARSET;
  $bits = '';
  $i = 0;
  while ($i < count($data)) {
  $bits = $bits . zfill(to_binary($data[$i]), 8);
  $i = $i + 1;
};
  $pad_bits = 0;
  if (fmod(strlen($bits), 6) != 0) {
  $pad_bits = 6 - fmod(strlen($bits), 6);
  $bits = $bits . repeat('0', $pad_bits);
}
  $j = 0;
  $encoded = '';
  while ($j < strlen($bits)) {
  $chunk = substr($bits, $j, $j + 6 - $j);
  $idx = from_binary($chunk);
  $encoded = $encoded . substr($B64_CHARSET, $idx, $idx + 1 - $idx);
  $j = $j + 6;
};
  $pad = _intdiv($pad_bits, 2);
  while ($pad > 0) {
  $encoded = $encoded . '=';
  $pad = $pad - 1;
};
  return $encoded;
}
function mochi_base64_decode($s) {
  global $B64_CHARSET;
  $padding = 0;
  $end = strlen($s);
  while ($end > 0 && substr($s, $end - 1, $end - ($end - 1)) == '=') {
  $padding = $padding + 1;
  $end = $end - 1;
};
  $bits = '';
  $k = 0;
  while ($k < $end) {
  $c = substr($s, $k, $k + 1 - $k);
  $idx = char_index($B64_CHARSET, $c);
  $bits = $bits . zfill(to_binary($idx), 6);
  $k = $k + 1;
};
  if ($padding > 0) {
  $bits = substr($bits, 0, strlen($bits) - $padding * 2 - 0);
}
  $bytes = [];
  $m = 0;
  while ($m < strlen($bits)) {
  $byte = from_binary(substr($bits, $m, $m + 8 - $m));
  $bytes = _append($bytes, $byte);
  $m = $m + 8;
};
  return $bytes;
}
function main() {
  global $B64_CHARSET;
  $data = [77, 111, 99, 104, 105];
  $encoded = mochi_base64_encode($data);
  echo rtrim($encoded), PHP_EOL;
  echo str_replace('    ', '  ', json_encode(mochi_base64_decode($encoded), 128)), PHP_EOL;
}
main();
