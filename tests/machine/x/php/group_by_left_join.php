<?php
$customers = [
    ["id" => 1, "name" => "Alice"],
    ["id" => 2, "name" => "Bob"],
    ["id" => 3, "name" => "Charlie"]
];
$orders = [
    ["id" => 100, "customerId" => 1],
    ["id" => 101, "customerId" => 1],
    ["id" => 102, "customerId" => 2]
];
$stats = (function() use ($customers, $orders) {
    $groups = [];
    foreach ($customers as $c) {
        $_found = false;
        foreach ($orders as $o) {
            if ($o['customerId'] == $c['id']) {
                $_found = true;
                $_k = json_encode($c['name']);
                $groups[$_k][] = ["c" => $c, "o" => $o];
            }
        }
        if (!$_found) {
            $o = null;
            $_k = json_encode($c['name']);
            $groups[$_k][] = ["c" => $c, "o" => $o];
        }
    }
    $result = [];
    foreach ($groups as $_k => $__g) {
        $_key = json_decode($_k, true);
        $g = ['key'=>$_key,'items'=> $__g];
        $result[] = [
    "name" => $g['key'],
    "count" => count((function() use ($g) {
        $result = [];
        foreach ($g['items'] as $r) {
            if ($r['o']) {
                $result[] = $r;
            }
        }
        return $result;
    })())
];
    }
    return $result;
})();
echo "--- Group Left Join ---", PHP_EOL;
foreach ($stats as $s) {
    var_dump($s['name'], "orders:", $s['count']);
}
?>
