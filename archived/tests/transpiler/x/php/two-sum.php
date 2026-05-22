<?php
function twoSum($nums, $target) {
  $n = count($nums);
  for ($i = 0; $i < $n; $i++) {
  for ($j = $i + 1; $j < $n; $j++) {
  if ($nums[$i] + $nums[$j] == $target) {
  return [$i, $j];
}
};
};
  return [-1, -1];
}
$result = twoSum([2, 7, 11, 15], 9);
echo (is_float($result[0]) ? json_encode($result[0], 1344) : $result[0]), PHP_EOL;
echo (is_float($result[1]) ? json_encode($result[1], 1344) : $result[1]), PHP_EOL;
