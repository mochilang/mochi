<?php
$people = [["name" => "Alice", "age" => 30], ["name" => "Bob", "age" => 25]];
foreach ($people as $_row) {
  $j = json_encode($_row);
  $j = str_replace(":", ": ", $j);
  $j = str_replace(",", ", ", $j);
  echo $j . PHP_EOL;
}
;
