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
function bit_xor($a, $b) {
  $ua = $a;
  $ub = $b;
  $res = 0;
  $bit = 1;
  while ($ua > 0 || $ub > 0) {
  $abit = $ua % 2;
  $bbit = $ub % 2;
  if (($abit == 1 && $bbit == 0) || ($abit == 0 && $bbit == 1)) {
  $res = $res + $bit;
}
  $ua = intval((_intdiv($ua, 2)));
  $ub = intval((_intdiv($ub, 2)));
  $bit = $bit * 2;
};
  return $res;
}
function find_unique_number($arr) {
  if (count($arr) == 0) {
  $panic('input list must not be empty');
}
  $result = 0;
  foreach ($arr as $num) {
  $result = bit_xor($result, $num);
};
  return $result;
}
echo rtrim(_str(find_unique_number([1, 1, 2, 2, 3]))), PHP_EOL;
echo rtrim(_str(find_unique_number([4, 5, 4, 6, 6]))), PHP_EOL;
echo rtrim(_str(find_unique_number([7]))), PHP_EOL;
