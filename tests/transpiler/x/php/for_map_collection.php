<?php
$m = ["a" => 1, "b" => 2];
foreach (array_keys($m) as $k) {
  echo (is_float($k) ? json_encode($k, 1344) : $k), PHP_EOL;
}
