<?php
$nums = [1, 2, 3];
$result = [];
foreach ($nums as $n) {
  if ($n > 1) {
    $result[] = array_sum($n);
  }
}

echo (is_float($result) ? json_encode($result, 1344) : $result), PHP_EOL;
