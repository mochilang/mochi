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
function prefix_sum($arr, $queries) {
  global $arr1, $queries1, $arr2, $queries2;
  $dp = [];
  $i = 0;
  while ($i < count($arr)) {
  if ($i == 0) {
  $dp = _append($dp, $arr[0]);
} else {
  $dp = _append($dp, $dp[$i - 1] + $arr[$i]);
}
  $i = $i + 1;
};
  $result = [];
  $j = 0;
  while ($j < count($queries)) {
  $q = $queries[$j];
  $sum = $dp[$q['right']];
  if ($q['left'] > 0) {
  $sum = $sum - $dp[$q['left'] - 1];
}
  $result = _append($result, $sum);
  $j = $j + 1;
};
  return $result;
}
$arr1 = [1, 4, 6, 2, 61, 12];
$queries1 = [['left' => 2, 'right' => 5], ['left' => 1, 'right' => 5], ['left' => 3, 'right' => 4]];
echo rtrim(_str(prefix_sum($arr1, $queries1))), PHP_EOL;
$arr2 = [4, 2, 1, 6, 3];
$queries2 = [['left' => 3, 'right' => 4], ['left' => 1, 'right' => 3], ['left' => 0, 'right' => 2]];
echo rtrim(_str(prefix_sum($arr2, $queries2))), PHP_EOL;
