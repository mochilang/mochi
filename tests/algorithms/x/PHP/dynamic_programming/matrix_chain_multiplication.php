<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$INF = 1000000000;
function matrix_chain_multiply($arr) {
  global $INF;
  if (count($arr) < 2) {
  return 0;
}
  $n = count($arr);
  $dp = [];
  $i = 0;
  while ($i < $n) {
  $row = [];
  $j = 0;
  while ($j < $n) {
  $row = _append($row, $INF);
  $j = $j + 1;
};
  $dp = _append($dp, $row);
  $i = $i + 1;
};
  $i = $n - 1;
  while ($i > 0) {
  $j = $i;
  while ($j < $n) {
  if ($i == $j) {
  $dp[$i][$j] = 0;
} else {
  $k = $i;
  while ($k < $j) {
  $cost = $dp[$i][$k] + $dp[$k + 1][$j] + $arr[$i - 1] * $arr[$k] * $arr[$j];
  if ($cost < $dp[$i][$j]) {
  $dp[$i][$j] = $cost;
}
  $k = $k + 1;
};
}
  $j = $j + 1;
};
  $i = $i - 1;
};
  return $dp[1][$n - 1];
}
