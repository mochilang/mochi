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
function max_cross_sum($arr, $low, $mid, $high) {
  $left_sum = -1000000000000000000.0;
  $max_left = -1;
  $sum = 0.0;
  $i = $mid;
  while ($i >= $low) {
  $sum = $sum + $arr[$i];
  if ($sum > $left_sum) {
  $left_sum = $sum;
  $max_left = $i;
}
  $i = $i - 1;
};
  $right_sum = -1000000000000000000.0;
  $max_right = -1;
  $sum = 0.0;
  $i = $mid + 1;
  while ($i <= $high) {
  $sum = $sum + $arr[$i];
  if ($sum > $right_sum) {
  $right_sum = $sum;
  $max_right = $i;
}
  $i = $i + 1;
};
  return ['start' => $max_left, 'end' => $max_right, 'sum' => $left_sum + $right_sum];
}
function max_subarray($arr, $low, $high) {
  if (count($arr) == 0) {
  return ['start' => -1, 'end' => -1, 'sum' => 0.0];
}
  if ($low == $high) {
  return ['start' => $low, 'end' => $high, 'sum' => $arr[$low]];
}
  $mid = _intdiv(($low + $high), 2);
  $left = max_subarray($arr, $low, $mid);
  $right = max_subarray($arr, $mid + 1, $high);
  $cross = max_cross_sum($arr, $low, $mid, $high);
  if ($left['sum'] >= $right['sum'] && $left['sum'] >= $cross['sum']) {
  return $left;
}
  if ($right['sum'] >= $left['sum'] && $right['sum'] >= $cross['sum']) {
  return $right;
}
  return $cross;
}
function show($res) {
  echo rtrim('[' . _str($res['start']) . ', ' . _str($res['end']) . ', ' . _str($res['sum']) . ']'), PHP_EOL;
}
function main() {
  $nums1 = [-2.0, 1.0, -3.0, 4.0, -1.0, 2.0, 1.0, -5.0, 4.0];
  $res1 = max_subarray($nums1, 0, count($nums1) - 1);
  show($res1);
  $nums2 = [2.0, 8.0, 9.0];
  $res2 = max_subarray($nums2, 0, count($nums2) - 1);
  show($res2);
  $nums3 = [0.0, 0.0];
  $res3 = max_subarray($nums3, 0, count($nums3) - 1);
  show($res3);
  $nums4 = [-1.0, 0.0, 1.0];
  $res4 = max_subarray($nums4, 0, count($nums4) - 1);
  show($res4);
  $nums5 = [-2.0, -3.0, -1.0, -4.0, -6.0];
  $res5 = max_subarray($nums5, 0, count($nums5) - 1);
  show($res5);
  $nums6 = [];
  $res6 = max_subarray($nums6, 0, 0);
  show($res6);
}
main();
