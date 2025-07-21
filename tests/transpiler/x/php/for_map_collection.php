<?php
$m = ["a" => 1, "b" => 2];
foreach (array_keys($m) as $k) {
  echo rtrim((is_float($k) ? sprintf("%.15f", $k) : $k)), PHP_EOL;
}
?>
