<?php
$customers = [
    ["id" => 1, "name" => "Alice"],
    ["id" => 2, "name" => "Bob"]
];
$orders = [
    ["id" => 100, "customerId" => 1],
    ["id" => 101, "customerId" => 1],
    ["id" => 102, "customerId" => 2]
];
$stats = (function() use ($customers, $orders) {
    $groups = [];
    foreach ($orders as $o) {
        foreach ($customers as $c) {
            if ($o['customerId'] == $c['id']) {
                $_k = json_encode($c['name']);
                $groups[$_k][] = ["o" => $o, "c" => $c];
            }
        }
    }
    $result = [];
    foreach ($groups as $_k => $__g) {
        $_key = json_decode($_k, true);
        $g = ['key'=>$_key,'items'=> $__g];
        $result[] = [
    "name" => $g['key'],
    "count" => count($g['items'])
];
    }
    return $result;
})();
echo "--- Orders per customer ---", PHP_EOL;
foreach ($stats as $s) {
    var_dump($s['name'], "orders:", $s['count']);
}
?>
