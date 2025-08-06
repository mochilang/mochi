<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function index_of_min($xs) {
  $min_idx = 0;
  $i = 1;
  while ($i < count($xs)) {
  if ($xs[$i] < $xs[$min_idx]) {
  $min_idx = $i;
}
  $i = $i + 1;
};
  return $min_idx;
}
function remove_at($xs, $idx) {
  $res = [];
  $i = 0;
  while ($i < count($xs)) {
  if ($i != $idx) {
  $res = _append($res, $xs[$i]);
}
  $i = $i + 1;
};
  return $res;
}
function optimal_merge_pattern($files) {
  $arr = $files;
  $optimal_merge_cost = 0;
  while (count($arr) > 1) {
  $temp = 0;
  $k = 0;
  while ($k < 2) {
  $min_idx = index_of_min($arr);
  $temp = $temp + $arr[$min_idx];
  $arr = remove_at($arr, $min_idx);
  $k = $k + 1;
};
  $arr = _append($arr, $temp);
  $optimal_merge_cost = $optimal_merge_cost + $temp;
};
  return $optimal_merge_cost;
}
echo rtrim(json_encode(optimal_merge_pattern([2, 3, 4]), 1344)), PHP_EOL;
echo rtrim(json_encode(optimal_merge_pattern([5, 10, 20, 30, 30]), 1344)), PHP_EOL;
echo rtrim(json_encode(optimal_merge_pattern([8, 8, 8, 8, 8]), 1344)), PHP_EOL;
