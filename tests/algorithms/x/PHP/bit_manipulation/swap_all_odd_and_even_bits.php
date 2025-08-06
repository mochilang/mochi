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
function pad_left_num($n) {
  $s = _str($n);
  while (strlen($s) < 5) {
  $s = ' ' . $s;
};
  return $s;
}
function to_binary($n) {
  $sign = '';
  $num = $n;
  if ($num < 0) {
  $sign = '-';
  $num = 0 - $num;
}
  $bits = '';
  while ($num > 0) {
  $bits = _str($num % 2) . $bits;
  $num = _intdiv(($num - ($num % 2)), 2);
};
  if ($bits == '') {
  $bits = '0';
}
  $min_width = 8;
  while (strlen($bits) < ($min_width - strlen($sign))) {
  $bits = '0' . $bits;
};
  return $sign . $bits;
}
function show_bits($before, $after) {
  return pad_left_num($before) . ': ' . to_binary($before) . '
' . pad_left_num($after) . ': ' . to_binary($after);
}
function lshift($num, $k) {
  $result = $num;
  $i = 0;
  while ($i < $k) {
  $result = $result * 2;
  $i = $i + 1;
};
  return $result;
}
function rshift($num, $k) {
  $result = $num;
  $i = 0;
  while ($i < $k) {
  $result = _intdiv(($result - ($result % 2)), 2);
  $i = $i + 1;
};
  return $result;
}
function swap_odd_even_bits($num) {
  $n = $num;
  if ($n < 0) {
  $n = $n + 4294967296;
}
  $result = 0;
  $i = 0;
  while ($i < 32) {
  $bit1 = fmod(rshift($n, $i), 2);
  $bit2 = fmod(rshift($n, $i + 1), 2);
  $result = $result + lshift($bit1, $i + 1) + lshift($bit2, $i);
  $i = $i + 2;
};
  return $result;
}
function main() {
  $nums = [-1, 0, 1, 2, 3, 4, 23, 24];
  $i = 0;
  while ($i < count($nums)) {
  $n = $nums[$i];
  echo rtrim(show_bits($n, swap_odd_even_bits($n))), PHP_EOL;
  echo rtrim(''), PHP_EOL;
  $i = $i + 1;
};
}
main();
