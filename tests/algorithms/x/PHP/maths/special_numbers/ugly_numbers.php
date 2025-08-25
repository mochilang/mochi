<?php
error_reporting(E_ALL & ~E_DEPRECATED);
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
function ugly_numbers($n) {
  if ($n <= 0) {
  return 1;
}
  $ugly_nums = [];
  $ugly_nums = _append($ugly_nums, 1);
  $i2 = 0;
  $i3 = 0;
  $i5 = 0;
  $next_2 = 2;
  $next_3 = 3;
  $next_5 = 5;
  $count = 1;
  while ($count < $n) {
  $next_num = ($next_2 < $next_3 ? ($next_2 < $next_5 ? $next_2 : $next_5) : ($next_3 < $next_5 ? $next_3 : $next_5));
  $ugly_nums = _append($ugly_nums, $next_num);
  if ($next_num == $next_2) {
  $i2 = $i2 + 1;
  $next_2 = $ugly_nums[$i2] * 2;
}
  if ($next_num == $next_3) {
  $i3 = $i3 + 1;
  $next_3 = $ugly_nums[$i3] * 3;
}
  if ($next_num == $next_5) {
  $i5 = $i5 + 1;
  $next_5 = $ugly_nums[$i5] * 5;
}
  $count = $count + 1;
};
  return $ugly_nums[count($ugly_nums) - 1];
}
echo rtrim(_str(ugly_numbers(100))), PHP_EOL;
echo rtrim(_str(ugly_numbers(0))), PHP_EOL;
echo rtrim(_str(ugly_numbers(20))), PHP_EOL;
echo rtrim(_str(ugly_numbers(-5))), PHP_EOL;
echo rtrim(_str(ugly_numbers(200))), PHP_EOL;
