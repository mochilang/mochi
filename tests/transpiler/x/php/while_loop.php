<?php
$i = 0;
while ($i < 3) {
  echo rtrim((is_float($i) ? sprintf("%.15f", $i) : $i)), PHP_EOL;
  $i = $i + 1;
}
?>
