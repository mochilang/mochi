<?php
$numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9];
foreach ($numbers as $n) {
  if ($n % 2 == 0) {
  continue;
}
  if ($n > 7) {
  break;
}
  echo "odd number:" . " " . (is_float($n) ? json_encode($n, 1344) : $n), PHP_EOL;
}
