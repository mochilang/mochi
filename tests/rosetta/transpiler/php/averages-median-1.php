<?php
ini_set('memory_limit', '-1');
function sortFloat($xs) {
  global $median;
  $arr = $xs;
  $n = count($arr);
  $i = 0;
  while ($i < $n) {
  $j = 0;
  while ($j < $n - 1) {
  if ($arr[$j] > $arr[$j + 1]) {
  $tmp = $arr[$j];
  $arr[$j] = $arr[$j + 1];
  $arr[$j + 1] = $tmp;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $arr;
}
function median($a) {
  global $sortFloat;
  $arr = sortFloat($a);
  $half = intval((count($arr) / 2));
  $m = $arr[$half];
  if (count($arr) % 2 == 0) {
  $m = ($m + $arr[$half - 1]) / 2.0;
}
  return $m;
}
echo rtrim(json_encode(median([3.0, 1.0, 4.0, 1.0]), 1344)), PHP_EOL;
echo rtrim(json_encode(median([3.0, 1.0, 4.0, 1.0, 5.0]), 1344)), PHP_EOL;
