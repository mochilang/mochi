<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function dp_count($s, $n) {
  if ($n < 0) {
  return 0;
}
  $table = [];
  $i = 0;
  while ($i <= $n) {
  $table = _append($table, 0);
  $i = $i + 1;
};
  $table[0] = 1;
  $idx = 0;
  while ($idx < count($s)) {
  $coin_val = $s[$idx];
  $j = $coin_val;
  while ($j <= $n) {
  $table[$j] = $table[$j] + $table[$j - $coin_val];
  $j = $j + 1;
};
  $idx = $idx + 1;
};
  return $table[$n];
}
echo rtrim(json_encode(dp_count([1, 2, 3], 4), 1344)), PHP_EOL;
echo rtrim(json_encode(dp_count([1, 2, 3], 7), 1344)), PHP_EOL;
echo rtrim(json_encode(dp_count([2, 5, 3, 6], 10), 1344)), PHP_EOL;
echo rtrim(json_encode(dp_count([10], 99), 1344)), PHP_EOL;
echo rtrim(json_encode(dp_count([4, 5, 6], 0), 1344)), PHP_EOL;
echo rtrim(json_encode(dp_count([1, 2, 3], -5), 1344)), PHP_EOL;
