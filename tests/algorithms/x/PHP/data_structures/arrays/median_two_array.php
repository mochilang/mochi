<?php
ini_set('memory_limit', '-1');
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
function sortFloats($xs) {
  $arr = $xs;
  $i = 0;
  while ($i < count($arr)) {
  $j = 0;
  while ($j < count($arr) - 1) {
  if ($arr[$j] > $arr[$j + 1]) {
  $t = $arr[$j];
  $arr[$j] = $arr[$j + 1];
  $arr[$j + 1] = $t;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $arr;
}
function find_median_sorted_arrays($nums1, $nums2) {
  if (count($nums1) == 0 && count($nums2) == 0) {
  $panic('Both input arrays are empty.');
}
  $merged = [];
  $i = 0;
  while ($i < count($nums1)) {
  $merged = _append($merged, $nums1[$i]);
  $i = $i + 1;
};
  $j = 0;
  while ($j < count($nums2)) {
  $merged = _append($merged, $nums2[$j]);
  $j = $j + 1;
};
  $sorted = sortFloats($merged);
  $total = count($sorted);
  if ($total % 2 == 1) {
  return $sorted[_intdiv($total, 2)];
}
  $middle1 = $sorted[_intdiv($total, 2) - 1];
  $middle2 = $sorted[_intdiv($total, 2)];
  return ($middle1 + $middle2) / 2.0;
}
echo rtrim(json_encode(find_median_sorted_arrays([1.0, 3.0], [2.0]), 1344)), PHP_EOL;
echo rtrim(json_encode(find_median_sorted_arrays([1.0, 2.0], [3.0, 4.0]), 1344)), PHP_EOL;
echo rtrim(json_encode(find_median_sorted_arrays([0.0, 0.0], [0.0, 0.0]), 1344)), PHP_EOL;
echo rtrim(json_encode(find_median_sorted_arrays([], [1.0]), 1344)), PHP_EOL;
echo rtrim(json_encode(find_median_sorted_arrays([-1000.0], [1000.0]), 1344)), PHP_EOL;
echo rtrim(json_encode(find_median_sorted_arrays([-1.1, -2.2], [-3.3, -4.4]), 1344)), PHP_EOL;
