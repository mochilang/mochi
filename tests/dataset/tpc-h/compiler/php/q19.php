<?php
$part = [
    [
        "p_partkey" => 1,
        "p_brand" => "Brand#12",
        "p_container" => "SM BOX",
        "p_size" => 3
    ],
    [
        "p_partkey" => 2,
        "p_brand" => "Brand#23",
        "p_container" => "MED BOX",
        "p_size" => 5
    ],
    [
        "p_partkey" => 3,
        "p_brand" => "Brand#34",
        "p_container" => "LG BOX",
        "p_size" => 15
    ]
];
$lineitem = [
    [
        "l_partkey" => 1,
        "l_quantity" => 5,
        "l_extendedprice" => 1000,
        "l_discount" => 0.1,
        "l_shipmode" => "AIR",
        "l_shipinstruct" => "DELIVER IN PERSON"
    ],
    [
        "l_partkey" => 2,
        "l_quantity" => 15,
        "l_extendedprice" => 2000,
        "l_discount" => 0.05,
        "l_shipmode" => "AIR REG",
        "l_shipinstruct" => "DELIVER IN PERSON"
    ],
    [
        "l_partkey" => 3,
        "l_quantity" => 35,
        "l_extendedprice" => 1500,
        "l_discount" => 0,
        "l_shipmode" => "AIR",
        "l_shipinstruct" => "DELIVER IN PERSON"
    ]
];
$revenues = (function() use ($lineitem, $part) {
    $result = [];
    foreach ($lineitem as $l) {
        foreach ($part as $p) {
            if ($p['p_partkey'] == $l['l_partkey']) {
                if (in_array(((($p['p_brand'] == "Brand#12") && (in_array($p['p_container'], [
    "SM CASE",
    "SM BOX",
    "SM PACK",
    "SM PKG"
])) && ($l['l_quantity'] >= 1 && $l['l_quantity'] <= 11) && ($p['p_size'] >= 1 && $p['p_size'] <= 5)) || (($p['p_brand'] == "Brand#23") && (in_array($p['p_container'], [
    "MED BAG",
    "MED BOX",
    "MED PKG",
    "MED PACK"
])) && ($l['l_quantity'] >= 10 && $l['l_quantity'] <= 20) && ($p['p_size'] >= 1 && $p['p_size'] <= 10)) || (($p['p_brand'] == "Brand#34") && (in_array($p['p_container'], [
    "LG CASE",
    "LG BOX",
    "LG PACK",
    "LG PKG"
])) && ($l['l_quantity'] >= 20 && $l['l_quantity'] <= 30) && ($p['p_size'] >= 1 && $p['p_size'] <= 15))) && $l['l_shipmode'], ["AIR", "AIR REG"]) && $l['l_shipinstruct'] == "DELIVER IN PERSON") {
                    $result[] = $l['l_extendedprice'] * (1 - $l['l_discount']);
                }
            }
        }
    }
    return $result;
})();
$result = array_sum($revenues);
echo json_encode($result), PHP_EOL;
?>
