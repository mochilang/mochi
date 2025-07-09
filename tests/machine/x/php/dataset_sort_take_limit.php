<?php
$products = [[$name => "Laptop", $price => 1500], [$name => "Smartphone", $price => 900], [$name => "Tablet", $price => 600], [$name => "Monitor", $price => 300], [$name => "Keyboard", $price => 100], [$name => "Mouse", $price => 50], [$name => "Headphones", $price => 200]];
$expensive = (function() {
    $result = [];
    foreach ($products as $p) {
        $result[] = [-$p->price, $p];
    }
    usort($result, function($a, $b) { return $a[0] <=> $b[0]; });
    $result = array_map(fn($r) => $r[1], $result);
    $result = array_slice($result, 1, 3);
    return $result;
})();
var_dump("--- Top products (excluding most expensive) ---");
foreach ($expensive as $item) {
    var_dump($item->name, "costs $", $item->price);
}
