<?php
ini_set('memory_limit', '-1');
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
function find_missing_number($nums) {
  $low = intval(min($nums));
  $high = intval(max($nums));
  $count = $high - $low + 1;
  $expected_sum = _intdiv(($low + $high) * $count, 2);
  $actual_sum = 0;
  $i = 0;
  $n = count($nums);
  while ($i < $n) {
  $actual_sum = $actual_sum + $nums[$i];
  $i = $i + 1;
};
  return $expected_sum - $actual_sum;
}
echo rtrim(json_encode(find_missing_number([0, 1, 3, 4]), 1344)), PHP_EOL;
