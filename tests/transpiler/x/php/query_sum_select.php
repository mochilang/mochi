<?php
$nums = [1, 2, 3];
$result = [];
foreach ($nums as $n) {
  if ($n > 1) {
    $result[] = array_sum($n);
  }
}

echo rtrim((is_float($result) ? sprintf("%.15f", $result) : $result)), PHP_EOL;
?>
