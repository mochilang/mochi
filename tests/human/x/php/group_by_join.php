<?php
$customers = [
    ["id" => 1, "name" => "Alice"],
    ["id" => 2, "name" => "Bob"],
];
$orders = [
    ["id" => 100, "customerId" => 1],
    ["id" => 101, "customerId" => 1],
    ["id" => 102, "customerId" => 2],
];
$groups = [];
foreach ($orders as $o) {
    foreach ($customers as $c) {
        if ($o['customerId'] === $c['id']) {
            $groups[$c['name']][] = $o;
        }
    }
}
ksort($groups);
$stats = [];
foreach ($groups as $name => $ordersFor) {
    $stats[] = ['name'=>$name, 'count'=>count($ordersFor)];
}
var_dump("--- Orders per customer ---");
foreach ($stats as $s) {
    var_dump($s['name'], "orders:", $s['count']);
}
?>
