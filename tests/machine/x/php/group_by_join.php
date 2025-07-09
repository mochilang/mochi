<?php
$customers = [["id" => 1, "name" => "Alice"], ["id" => 2, "name" => "Bob"]];
$orders = [["id" => 100, "customerId" => 1], ["id" => 101, "customerId" => 1], ["id" => 102, "customerId" => 2]];
$stats = (function() {
    $groups = [];
    foreach ($orders as $o) {
        foreach ($customers as $c) {
            if ($o->customerId == $c->id) {
                $_k = json_encode($c->name);
                $groups[$_k][] = $o;
            }
        }
    }
    $result = [];
    foreach ($groups as $_k => $__g) {
        $g = ['key'=>json_decode($_k, true),'items'=> $__g];
        $result[] = ["name" => $g->key, "count" => count($g['items'])];
    }
    return $result;
})();
var_dump("--- Orders per customer ---");
foreach ($stats as $s) {
    var_dump($s->name, "orders:", $s->count);
}
