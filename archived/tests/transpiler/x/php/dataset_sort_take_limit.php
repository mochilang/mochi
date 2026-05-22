<?php
$products = [["name" => "Laptop", "price" => 1500], ["name" => "Smartphone", "price" => 900], ["name" => "Tablet", "price" => 600], ["name" => "Monitor", "price" => 300], ["name" => "Keyboard", "price" => 100], ["name" => "Mouse", "price" => 50], ["name" => "Headphones", "price" => 200]];
$expensive = [];
foreach ($products as $p) {
  $expensive[] = $p;
}

echo "--- Top products (excluding most expensive) ---", PHP_EOL;
foreach ($expensive as $item) {
  echo (is_float($item["name"]) ? json_encode($item["name"], 1344) : $item["name"]) . " " . "costs $" . " " . (is_float($item["price"]) ? json_encode($item["price"], 1344) : $item["price"]), PHP_EOL;
}
