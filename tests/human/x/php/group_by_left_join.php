<?php
$customers = [
    ["id" => 1, "name" => "Alice"],
    ["id" => 2, "name" => "Bob"],
    ["id" => 3, "name" => "Charlie"],
];
$orders = [
    ["id" => 100, "customerId" => 1],
    ["id" => 101, "customerId" => 1],
    ["id" => 102, "customerId" => 2],
];
$stats = [];
foreach ($customers as $c) {
    $count = 0;
    foreach ($orders as $o) {
        if ($o['customerId'] === $c['id']) {
            $count++;
        }
    }
    $stats[] = ['name'=>$c['name'], 'count'=>$count];
}
var_dump("--- Group Left Join ---");
foreach ($stats as $s) {
    var_dump($s['name'], "orders:", $s['count']);
}
?>
