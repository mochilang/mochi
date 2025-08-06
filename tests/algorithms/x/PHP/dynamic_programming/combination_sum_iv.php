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
function make_list($len, $value) {
  $arr = [];
  $i = 0;
  while ($i < $len) {
  $arr = _append($arr, $value);
  $i = $i + 1;
};
  return $arr;
}
function count_recursive($array, $target) {
  if ($target < 0) {
  return 0;
}
  if ($target == 0) {
  return 1;
}
  $total = 0;
  $i = 0;
  while ($i < count($array)) {
  $total = $total + count_recursive($array, $target - $array[$i]);
  $i = $i + 1;
};
  return $total;
}
function combination_sum_iv($array, $target) {
  return count_recursive($array, $target);
}
function count_dp($array, $target, &$dp) {
  if ($target < 0) {
  return 0;
}
  if ($target == 0) {
  return 1;
}
  if ($dp[$target] > (0 - 1)) {
  return $dp[$target];
}
  $total = 0;
  $i = 0;
  while ($i < count($array)) {
  $total = $total + count_dp($array, $target - $array[$i], $dp);
  $i = $i + 1;
};
  $dp[$target] = $total;
  return $total;
}
function combination_sum_iv_dp_array($array, $target) {
  $dp = make_list($target + 1, -1);
  return count_dp($array, $target, $dp);
}
function combination_sum_iv_bottom_up($n, $array, $target) {
  $dp = make_list($target + 1, 0);
  $dp[0] = 1;
  $i = 1;
  while ($i <= $target) {
  $j = 0;
  while ($j < $n) {
  if ($i - $array[$j] >= 0) {
  $dp[$i] = $dp[$i] + $dp[$i - $array[$j]];
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $dp[$target];
}
echo rtrim(_str(combination_sum_iv([1, 2, 5], 5))), PHP_EOL;
echo rtrim(_str(combination_sum_iv_dp_array([1, 2, 5], 5))), PHP_EOL;
echo rtrim(_str(combination_sum_iv_bottom_up(3, [1, 2, 5], 5))), PHP_EOL;
