<?php
// customers: [{string: any}]
$customers = [["id" => 1, "name" => "Alice"], ["id" => 2, "name" => "Bob"], ["id" => 3, "name" => "Charlie"]];
// orders: [{string: int}]
$orders = [["id" => 100, "customerId" => 1, "total" => 250], ["id" => 101, "customerId" => 2, "total" => 125], ["id" => 102, "customerId" => 1, "total" => 300]];
// result: [{string: any}]
$result = (function() use ($customers, $orders) {
	$res = [];
	foreach ((is_string($orders) ? str_split($orders) : $orders) as $o) {
		foreach ((is_string($customers) ? str_split($customers) : $customers) as $c) {
			$res[] = ["orderId" => $o['id'], "orderCustomerId" => $o['customerId'], "pairedCustomerName" => $c['name'], "orderTotal" => $o['total']];
		}
	}
	return $res;
})();
_print("--- Cross Join: All order-customer pairs ---");
foreach ((is_string($result) ? str_split($result) : $result) as $entry) {
	_print("Order", $entry['orderId'], "(customerId:", $entry['orderCustomerId'], ", total: $", $entry['orderTotal'], ") paired with", $entry['pairedCustomerName']);
}

function _print(...$args) {
    $parts = [];
    foreach ($args as $a) {
        if (is_null($a)) { $parts[] = '<nil>'; }
        elseif (is_array($a) || is_object($a)) { $parts[] = json_encode($a); } else { $parts[] = strval($a); }
    }
    echo implode(' ', $parts), PHP_EOL;
}
