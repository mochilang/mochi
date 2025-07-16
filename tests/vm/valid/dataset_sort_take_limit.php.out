<?php
$products = [
    ["name" => "Laptop", "price" => 1500],
    ["name" => "Smartphone", "price" => 900],
    ["name" => "Tablet", "price" => 600],
    ["name" => "Monitor", "price" => 300],
    ["name" => "Keyboard", "price" => 100],
    ["name" => "Mouse", "price" => 50],
    ["name" => "Headphones", "price" => 200]
];
$expensive = (function() use ($products) {
    $result = [];
    foreach ($products as $p) {
        $result[] = [-$p['price'], $p];
    }
    usort($result, function($a, $b) { return $a[0] <=> $b[0]; });
    $result = array_map(fn($r) => $r[1], $result);
    $result = array_slice($result, 1, 3);
    return $result;
})();
_print("--- Top products (excluding most expensive) ---");
foreach ($expensive as $item) {
    _print($item['name'], "costs $", $item['price']);
}
function _print(...$args) {
    $first = true;
    foreach ($args as $a) {
        if (!$first) echo ' ';
        $first = false;
        if (is_array($a)) {
            if (array_is_list($a)) {
                if ($a && is_array($a[0])) {
                    $parts = [];
                    foreach ($a as $sub) {
                        if (is_array($sub)) {
                            $parts[] = '[' . implode(' ', $sub) . ']';
                        } else {
                            $parts[] = strval($sub);
                        }
                    }
                    echo implode(' ', $parts);
                } else {
                    echo '[' . implode(' ', array_map('strval', $a)) . ']';
                }
            } else {
                echo json_encode($a);
            }
        } else {
            echo strval($a);
        }
    }
    echo PHP_EOL;
}
?>
