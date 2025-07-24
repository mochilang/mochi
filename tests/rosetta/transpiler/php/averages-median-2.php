<?php
ini_set('memory_limit', '-1');
function sel($list, $k) {
  global $median;
  $i = 0;
  while ($i <= $k) {
  $minIndex = $i;
  $j = $i + 1;
  while ($j < count($list)) {
  if ($list[$j] < $list[$minIndex]) {
  $minIndex = $j;
}
  $j = $j + 1;
};
  $tmp = $list[$i];
  $list[$i] = $list[$minIndex];
  $list[$minIndex] = $tmp;
  $i = $i + 1;
};
  return $list[$k];
}
function median($a) {
  global $sel;
  $arr = $a;
  $half = intval((count($arr) / 2));
  $med = sel($arr, $half);
  if (count($arr) % 2 == 0) {
  return ($med + $arr[$half - 1]) / 2.0;
}
  return $med;
}
echo rtrim(json_encode(median([3.0, 1.0, 4.0, 1.0]), 1344)), PHP_EOL;
echo rtrim(json_encode(median([3.0, 1.0, 4.0, 1.0, 5.0]), 1344)), PHP_EOL;
