<?php
foreach ([1, 2, 3] as $n) {
  echo rtrim((is_float($n) ? sprintf("%.15f", $n) : $n)), PHP_EOL;
}
?>
