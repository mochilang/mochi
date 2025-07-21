<?php
for ($i = 1; $i < 4; $i++) {
  echo rtrim((is_float($i) ? sprintf("%.15f", $i) : $i)), PHP_EOL;
}
?>
