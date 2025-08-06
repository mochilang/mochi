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
function max_subarray_sum($nums, $allow_empty) {
  global $empty;
  if (count($nums) == 0) {
  return 0.0;
}
  $max_sum = 0.0;
  $curr_sum = 0.0;
  if ($allow_empty) {
  $max_sum = 0.0;
  $curr_sum = 0.0;
  $i = 0;
  while ($i < count($nums)) {
  $num = $nums[$i];
  $temp = $curr_sum + $num;
  $curr_sum = ($temp > 0.0 ? $temp : 0.0);
  if ($curr_sum > $max_sum) {
  $max_sum = $curr_sum;
}
  $i = $i + 1;
};
} else {
  $max_sum = $nums[0];
  $curr_sum = $nums[0];
  $i = 1;
  while ($i < count($nums)) {
  $num = $nums[$i];
  $temp = $curr_sum + $num;
  $curr_sum = ($temp > $num ? $temp : $num);
  if ($curr_sum > $max_sum) {
  $max_sum = $curr_sum;
}
  $i = $i + 1;
};
}
  return $max_sum;
}
echo rtrim(_str(max_subarray_sum([2.0, 8.0, 9.0], false))), PHP_EOL;
echo rtrim(_str(max_subarray_sum([0.0, 0.0], false))), PHP_EOL;
echo rtrim(_str(max_subarray_sum([-1.0, 0.0, 1.0], false))), PHP_EOL;
echo rtrim(_str(max_subarray_sum([1.0, 2.0, 3.0, 4.0, -2.0], false))), PHP_EOL;
echo rtrim(_str(max_subarray_sum([-2.0, 1.0, -3.0, 4.0, -1.0, 2.0, 1.0, -5.0, 4.0], false))), PHP_EOL;
echo rtrim(_str(max_subarray_sum([2.0, 3.0, -9.0, 8.0, -2.0], false))), PHP_EOL;
echo rtrim(_str(max_subarray_sum([-2.0, -3.0, -1.0, -4.0, -6.0], false))), PHP_EOL;
echo rtrim(_str(max_subarray_sum([-2.0, -3.0, -1.0, -4.0, -6.0], true))), PHP_EOL;
$empty = [];
echo rtrim(_str(max_subarray_sum($empty, false))), PHP_EOL;
