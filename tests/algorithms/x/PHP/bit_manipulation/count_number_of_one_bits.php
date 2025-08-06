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
function bit_and($a, $b) {
  $ua = $a;
  $ub = $b;
  $res = 0;
  $bit = 1;
  while ($ua > 0 || $ub > 0) {
  if ($ua % 2 == 1 && $ub % 2 == 1) {
  $res = $res + $bit;
}
  $ua = intval((_intdiv($ua, 2)));
  $ub = intval((_intdiv($ub, 2)));
  $bit = $bit * 2;
};
  return $res;
}
function count_bits_kernighan($n) {
  if ($n < 0) {
  $panic('the value of input must not be negative');
}
  $num = $n;
  $result = 0;
  while ($num != 0) {
  $num = bit_and($num, $num - 1);
  $result = $result + 1;
};
  return $result;
}
function count_bits_modulo($n) {
  if ($n < 0) {
  $panic('the value of input must not be negative');
}
  $num = $n;
  $result = 0;
  while ($num != 0) {
  if ($num % 2 == 1) {
  $result = $result + 1;
}
  $num = intval((_intdiv($num, 2)));
};
  return $result;
}
function main() {
  $numbers = [25, 37, 21, 58, 0, 256];
  $i = 0;
  while ($i < count($numbers)) {
  echo rtrim(_str(count_bits_kernighan($numbers[$i]))), PHP_EOL;
  $i = $i + 1;
};
  $i = 0;
  while ($i < count($numbers)) {
  echo rtrim(_str(count_bits_modulo($numbers[$i]))), PHP_EOL;
  $i = $i + 1;
};
}
main();
