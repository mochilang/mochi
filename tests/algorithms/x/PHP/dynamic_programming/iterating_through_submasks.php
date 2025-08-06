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
function bitwise_and($a, $b) {
  $result = 0;
  $bit = 1;
  $x = $a;
  $y = $b;
  while ($x > 0 || $y > 0) {
  $abit = $x % 2;
  $bbit = $y % 2;
  if ($abit == 1 && $bbit == 1) {
  $result = $result + $bit;
}
  $x = _intdiv($x, 2);
  $y = _intdiv($y, 2);
  $bit = $bit * 2;
};
  return $result;
}
function list_of_submasks($mask) {
  if ($mask <= 0) {
  $panic('mask needs to be positive integer, your input ' . _str($mask));
}
  $all_submasks = [];
  $submask = $mask;
  while ($submask != 0) {
  $all_submasks = _append($all_submasks, $submask);
  $submask = bitwise_and($submask - 1, $mask);
};
  return $all_submasks;
}
echo rtrim(_str(list_of_submasks(15))), PHP_EOL;
echo rtrim(_str(list_of_submasks(13))), PHP_EOL;
