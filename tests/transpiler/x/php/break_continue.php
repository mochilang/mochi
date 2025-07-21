<?php
$numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9];
foreach ($numbers as $n) {
  if ($n % 2 == 0) {
  continue;
}
  if ($n > 7) {
  break;
}
  echo rtrim("odd number:" . " " . (is_float($n) ? sprintf("%.15f", $n) : $n)), PHP_EOL;
}
?>
