<?php
$products = [
    ["name"=>"Laptop","price"=>1500],
    ["name"=>"Smartphone","price"=>900],
    ["name"=>"Tablet","price"=>600],
    ["name"=>"Monitor","price"=>300],
    ["name"=>"Keyboard","price"=>100],
    ["name"=>"Mouse","price"=>50],
    ["name"=>"Headphones","price"=>200],
];
usort($products,function($a,$b){return $b['price'] <=> $a['price'];});
$expensive = array_slice($products,1,3);
_print("--- Top products (excluding most expensive) ---");
foreach ($expensive as $item) {
    _print($item['name'], "costs $", $item['price']);
}

function _print(...$args) {
    $parts = [];
    foreach ($args as $a) {
        if (is_array($a) || is_object($a)) {
            $parts[] = json_encode($a);
        } else {
            $parts[] = strval($a);
        }
    }
    echo implode(' ', $parts), PHP_EOL;
}
?>
