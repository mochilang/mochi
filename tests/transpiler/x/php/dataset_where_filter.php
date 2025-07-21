<?php
$people = [["name" => "Alice", "age" => 30], ["name" => "Bob", "age" => 15], ["name" => "Charlie", "age" => 65], ["name" => "Diana", "age" => 45]];
$adults = [];
foreach ($people as $person) {
  if ($person["age"] >= 18) {
    $adults[] = ["name" => $person["name"], "age" => $person["age"], "is_senior" => $person["age"] >= 60];
  }
}

echo rtrim("--- Adults ---"), PHP_EOL;
foreach ($adults as $person) {
  echo rtrim((is_float($person["name"]) ? sprintf("%.15f", $person["name"]) : $person["name"]) . " " . "is" . " " . (is_float($person["age"]) ? sprintf("%.15f", $person["age"]) : $person["age"]) . " " . ($person["is_senior"] ? " (senior)" : "")), PHP_EOL;
}
?>
