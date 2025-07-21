<?php
$products = [["name" => "Laptop", "price" => 1500], ["name" => "Smartphone", "price" => 900], ["name" => "Tablet", "price" => 600], ["name" => "Monitor", "price" => 300], ["name" => "Keyboard", "price" => 100], ["name" => "Mouse", "price" => 50], ["name" => "Headphones", "price" => 200]];
$expensive = [];
foreach ($products as $p) {
  $expensive[] = $p;
}

echo rtrim("--- Top products (excluding most expensive) ---"), PHP_EOL;
foreach ($expensive as $item) {
  echo rtrim($item["name"] . " " . "costs $" . " " . $item["price"]), PHP_EOL;
}
?>
