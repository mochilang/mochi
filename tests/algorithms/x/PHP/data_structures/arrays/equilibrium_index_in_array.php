<?php
ini_set('memory_limit', '-1');
function equilibrium_index($arr) {
  global $arr1, $arr2, $arr3, $arr4;
  $total = 0;
  $i = 0;
  while ($i < count($arr)) {
  $total = $total + $arr[$i];
  $i = $i + 1;
};
  $left = 0;
  $i = 0;
  while ($i < count($arr)) {
  $total = $total - $arr[$i];
  if ($left == $total) {
  return $i;
}
  $left = $left + $arr[$i];
  $i = $i + 1;
};
  return -1;
}
$arr1 = [-7, 1, 5, 2, -4, 3, 0];
echo rtrim(json_encode(equilibrium_index($arr1), 1344)), PHP_EOL;
$arr2 = [1, 2, 3, 4, 5];
echo rtrim(json_encode(equilibrium_index($arr2), 1344)), PHP_EOL;
$arr3 = [1, 1, 1, 1, 1];
echo rtrim(json_encode(equilibrium_index($arr3), 1344)), PHP_EOL;
$arr4 = [2, 4, 6, 8, 10, 3];
echo rtrim(json_encode(equilibrium_index($arr4), 1344)), PHP_EOL;
