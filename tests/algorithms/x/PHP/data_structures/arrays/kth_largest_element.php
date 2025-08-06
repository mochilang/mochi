<?php
ini_set('memory_limit', '-1');
function partition(&$arr, $low, $high) {
  global $arr1, $arr2;
  $pivot = $arr[$high];
  $i = $low - 1;
  $j = $low;
  while ($j < $high) {
  if ($arr[$j] >= $pivot) {
  $i = $i + 1;
  $tmp = $arr[$i];
  $arr[$i] = $arr[$j];
  $arr[$j] = $tmp;
}
  $j = $j + 1;
};
  $k = $i + 1;
  $tmp = $arr[$k];
  $arr[$k] = $arr[$high];
  $arr[$high] = $tmp;
  return $k;
}
function kth_largest_element(&$arr, $position) {
  global $arr1, $arr2;
  if (count($arr) == 0) {
  return -1;
}
  if ($position < 1 || $position > count($arr)) {
  return -1;
}
  $low = 0;
  $high = count($arr) - 1;
  while ($low <= $high) {
  if ($low > count($arr) - 1 || $high < 0) {
  return -1;
}
  $pivot_index = partition($arr, $low, $high);
  if ($pivot_index == $position - 1) {
  return $arr[$pivot_index];
} else {
  if ($pivot_index > $position - 1) {
  $high = $pivot_index - 1;
} else {
  $low = $pivot_index + 1;
};
}
};
  return -1;
}
$arr1 = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5];
echo rtrim(json_encode(kth_largest_element($arr1, 3), 1344)), PHP_EOL;
echo rtrim('
'), PHP_EOL;
$arr2 = [2, 5, 6, 1, 9, 3, 8, 4, 7, 3, 5];
echo rtrim(json_encode(kth_largest_element($arr2, 1), 1344)), PHP_EOL;
