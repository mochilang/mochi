<?php
ini_set('memory_limit', '-1');
function insertion_sort(&$a) {
  $i = 1;
  while ($i < count($a)) {
  $key = $a[$i];
  $j = $i - 1;
  while ($j >= 0 && $a[$j] > $key) {
  $a[$j + 1] = $a[$j];
  $j = $j - 1;
};
  $a[$j + 1] = $key;
  $i = $i + 1;
};
  return $a;
}
function minimum_waiting_time(&$queries) {
  $n = count($queries);
  if ($n == 0 || $n == 1) {
  return 0;
}
  $sorted = insertion_sort($queries);
  $total = 0;
  $i = 0;
  while ($i < $n) {
  $total = $total + $sorted[$i] * ($n - $i - 1);
  $i = $i + 1;
};
  return $total;
}
echo rtrim(json_encode(minimum_waiting_time([3, 2, 1, 2, 6]), 1344)), PHP_EOL;
echo rtrim(json_encode(minimum_waiting_time([3, 2, 1]), 1344)), PHP_EOL;
echo rtrim(json_encode(minimum_waiting_time([1, 2, 3, 4]), 1344)), PHP_EOL;
echo rtrim(json_encode(minimum_waiting_time([5, 5, 5, 5]), 1344)), PHP_EOL;
echo rtrim(json_encode(minimum_waiting_time([]), 1344)), PHP_EOL;
