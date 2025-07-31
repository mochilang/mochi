<?php
$customers = [["id" => 1, "name" => "Alice"], ["id" => 2, "name" => "Bob"], ["id" => 3, "name" => "Charlie"], ["id" => 4, "name" => "Diana"]];
$orders = [["id" => 100, "customerId" => 1, "total" => 250], ["id" => 101, "customerId" => 2, "total" => 125], ["id" => 102, "customerId" => 1, "total" => 300], ["id" => 103, "customerId" => 5, "total" => 80]];
$result = (function() use ($customers, $orders) {
    $result = [];
    foreach ($orders as $o) {
        ;
        foreach ($customers as $c) {
            if (!($o["customerId"] == $c["id"])) {
            }
            ;
            $result[] = ["order" => $o, "customer" => $c];
        }
        if (!$matched) {
            ;
            $result[] = ["order" => $o, "customer" => $c];
        }
    }
    foreach ($customers as $c) {
        ;
        foreach ($orders as $o) {
            if (!($o["customerId"] == $c["id"])) {
            }
            ;
            break;
        }
        if (!$matched) {
            ;
            $result[] = ["order" => $o, "customer" => $c];
        }
    }
    return $result;
}
)();
echo "--- Outer Join using syntax ---", PHP_EOL;
foreach ($result as $row) {
    if ($row["order"]) {
        if ($row["customer"]) {
            echo  . (is_float($row["order"]["total"]) ? json_encode($row["order"]["total"], 1344) : $row["order"]["total"]), PHP_EOL;
        }
        else {
            {
                echo  . (is_float($row["order"]["total"]) ? json_encode($row["order"]["total"], 1344) : $row["order"]["total"]), PHP_EOL;
            }
        }
    }
    else {
        {
            echo  . "has no orders", PHP_EOL;
        }
    }
}
