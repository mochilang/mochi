<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function sum_list($nums) {
  $s = 0;
  foreach ($nums as $n) {
  $s = $s + $n;
};
  return $s;
}
function create_state_space_tree($nums, $max_sum, $num_index, $path, $curr_sum, $remaining_sum) {
  $result = [];
  if ($curr_sum > $max_sum || $curr_sum + $remaining_sum < $max_sum) {
  return $result;
}
  if ($curr_sum == $max_sum) {
  $result = _append($result, $path);
  return $result;
}
  $index = $num_index;
  while ($index < count($nums)) {
  $value = $nums[$index];
  $subres = create_state_space_tree($nums, $max_sum, $index + 1, _append($path, $value), $curr_sum + $value, $remaining_sum - $value);
  $j = 0;
  while ($j < count($subres)) {
  $result = _append($result, $subres[$j]);
  $j = $j + 1;
};
  $index = $index + 1;
};
  return $result;
}
function generate_sum_of_subsets_solutions($nums, $max_sum) {
  $total = sum_list($nums);
  return create_state_space_tree($nums, $max_sum, 0, [], 0, $total);
}
function main() {
  echo str_replace('    ', '  ', json_encode(generate_sum_of_subsets_solutions([3, 34, 4, 12, 5, 2], 9), 128)), PHP_EOL;
}
main();
