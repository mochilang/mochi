<?php
ini_set('memory_limit', '-1');
function pairs_with_sum($arr, $req_sum) {
  $n = count($arr);
  $count = 0;
  $i = 0;
  while ($i < $n) {
  $j = $i + 1;
  while ($j < $n) {
  if ($arr[$i] + $arr[$j] == $req_sum) {
  $count = $count + 1;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $count;
}
echo rtrim(json_encode(pairs_with_sum([1, 5, 7, 1], 6), 1344)), PHP_EOL;
echo rtrim(json_encode(pairs_with_sum([1, 1, 1, 1, 1, 1, 1, 1], 2), 1344)), PHP_EOL;
echo rtrim(json_encode(pairs_with_sum([1, 7, 6, 2, 5, 4, 3, 1, 9, 8], 7), 1344)), PHP_EOL;
