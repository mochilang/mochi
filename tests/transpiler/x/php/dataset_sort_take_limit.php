<?php
$products = [["name" => "Laptop", "price" => 1500], ["name" => "Smartphone", "price" => 900], ["name" => "Tablet", "price" => 600], ["name" => "Monitor", "price" => 300], ["name" => "Keyboard", "price" => 100], ["name" => "Mouse", "price" => 50], ["name" => "Headphones", "price" => 200]];
$expensive = [];
foreach ($products as $p) {
  $expensive[] = $p;
}

echo rtrim("--- Top products (excluding most expensive) ---"), PHP_EOL;
foreach ($expensive as $item) {
  echo rtrim((is_float($item["name"]) ? sprintf("%.15f", $item["name"]) : $item["name"]) . " " . "costs $" . " " . (is_float($item["price"]) ? sprintf("%.15f", $item["price"]) : $item["price"])), PHP_EOL;
}
?>
