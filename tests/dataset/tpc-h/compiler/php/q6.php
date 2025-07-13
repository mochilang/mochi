<?php
$lineitem = [
    [
        "l_extendedprice" => 1000,
        "l_discount" => 0.06,
        "l_shipdate" => "1994-02-15",
        "l_quantity" => 10
    ],
    [
        "l_extendedprice" => 500,
        "l_discount" => 0.07,
        "l_shipdate" => "1994-03-10",
        "l_quantity" => 23
    ],
    [
        "l_extendedprice" => 400,
        "l_discount" => 0.04,
        "l_shipdate" => "1994-04-10",
        "l_quantity" => 15
    ],
    [
        "l_extendedprice" => 200,
        "l_discount" => 0.06,
        "l_shipdate" => "1995-01-01",
        "l_quantity" => 5
    ]
];
$result = (function() use ($lineitem) {
    $result = 0;
    foreach ($lineitem as $l) {
        if (($l['l_shipdate'] >= "1994-01-01") && ($l['l_shipdate'] < "1995-01-01") && ($l['l_discount'] >= 0.05) && ($l['l_discount'] <= 0.07) && ($l['l_quantity'] < 24)) {
            $result += $l['l_extendedprice'] * $l['l_discount'];
        }
    }
    return $result;
})();
echo json_encode($result), PHP_EOL;
?>
