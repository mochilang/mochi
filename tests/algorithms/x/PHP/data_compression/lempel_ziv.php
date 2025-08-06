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
  global $data;
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
function contains_key_int($m, $key) {
  global $data;
  foreach (array_keys($m) as $k) {
  if ($k == $key) {
  return true;
}
};
  return false;
}
function lzw_compress($bits) {
  global $data;
  $dict = ['0' => 0, '1' => 1];
  $current = '';
  $result = '';
  $index = 2;
  $i = 0;
  while ($i < strlen($bits)) {
  $ch = substr($bits, $i, $i + 1 - $i);
  $candidate = $current . $ch;
  if (contains_key_int($dict, $candidate)) {
  $current = $candidate;
} else {
  $result = $result . to_binary($dict[$current]);
  $dict[$candidate] = $index;
  $index = $index + 1;
  $current = $ch;
}
  $i = $i + 1;
};
  if ($current != '') {
  $result = $result . to_binary($dict[$current]);
}
  return $result;
}
$data = '01001100100111';
echo rtrim(lzw_compress($data)), PHP_EOL;
