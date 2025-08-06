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
function find_min($numbers) {
  $n = count($numbers);
  $s = 0;
  $idx = 0;
  while ($idx < $n) {
  $s = $s + $numbers[$idx];
  $idx = $idx + 1;
};
  $dp = [];
  $i = 0;
  while ($i <= $n) {
  $row = [];
  $j = 0;
  while ($j <= $s) {
  $row = _append($row, false);
  $j = $j + 1;
};
  $dp = _append($dp, $row);
  $i = $i + 1;
};
  $i = 0;
  while ($i <= $n) {
  $dp[$i][0] = true;
  $i = $i + 1;
};
  $j = 1;
  while ($j <= $s) {
  $dp[0][$j] = false;
  $j = $j + 1;
};
  $i = 1;
  while ($i <= $n) {
  $j = 1;
  while ($j <= $s) {
  $dp[$i][$j] = $dp[$i - 1][$j];
  if ($numbers[$i - 1] <= $j) {
  if ($dp[$i - 1][$j - $numbers[$i - 1]]) {
  $dp[$i][$j] = true;
};
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  $diff = 0;
  $j = _intdiv($s, 2);
  while ($j >= 0) {
  if ($dp[$n][$j]) {
  $diff = $s - 2 * $j;
  break;
}
  $j = $j - 1;
};
  return $diff;
}
echo rtrim(_str(find_min([1, 2, 3, 4, 5]))), PHP_EOL;
echo rtrim(_str(find_min([5, 5, 5, 5, 5]))), PHP_EOL;
echo rtrim(_str(find_min([5, 5, 5, 5]))), PHP_EOL;
echo rtrim(_str(find_min([3]))), PHP_EOL;
echo rtrim(_str(find_min([]))), PHP_EOL;
echo rtrim(_str(find_min([1, 2, 3, 4]))), PHP_EOL;
echo rtrim(_str(find_min([0, 0, 0, 0]))), PHP_EOL;
echo rtrim(_str(find_min([-1, -5, 5, 1]))), PHP_EOL;
echo rtrim(_str(find_min([9, 9, 9, 9, 9]))), PHP_EOL;
echo rtrim(_str(find_min([1, 5, 10, 3]))), PHP_EOL;
echo rtrim(_str(find_min([-1, 0, 1]))), PHP_EOL;
echo rtrim(_str(find_min([10, 9, 8, 7, 6, 5, 4, 3, 2, 1]))), PHP_EOL;
