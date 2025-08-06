<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function partition($m) {
  $memo = [];
  $i = 0;
  while ($i < $m + 1) {
  $row = [];
  $j = 0;
  while ($j < $m) {
  $row = _append($row, 0);
  $j = $j + 1;
};
  $memo = _append($memo, $row);
  $i = $i + 1;
};
  $i = 0;
  while ($i < $m + 1) {
  $memo[$i][0] = 1;
  $i = $i + 1;
};
  $n = 0;
  while ($n < $m + 1) {
  $k = 1;
  while ($k < $m) {
  $memo[$n][$k] = $memo[$n][$k] + $memo[$n][$k - 1];
  if ($n - $k > 0) {
  $memo[$n][$k] = $memo[$n][$k] + $memo[$n - $k - 1][$k];
}
  $k = $k + 1;
};
  $n = $n + 1;
};
  return $memo[$m][$m - 1];
}
echo rtrim(json_encode(partition(5), 1344)), PHP_EOL;
echo rtrim(json_encode(partition(7), 1344)), PHP_EOL;
echo rtrim(json_encode(partition(100), 1344)), PHP_EOL;
