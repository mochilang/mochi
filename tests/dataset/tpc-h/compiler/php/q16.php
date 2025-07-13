<?php
$supplier = [
    [
        "s_suppkey" => 100,
        "s_name" => "AlphaSupply",
        "s_address" => "123 Hilltop",
        "s_comment" => "Reliable and efficient"
    ],
    [
        "s_suppkey" => 200,
        "s_name" => "BetaSupply",
        "s_address" => "456 Riverside",
        "s_comment" => "Known for Customer Complaints"
    ]
];
$part = [
    [
        "p_partkey" => 1,
        "p_brand" => "Brand#12",
        "p_type" => "SMALL ANODIZED",
        "p_size" => 5
    ],
    [
        "p_partkey" => 2,
        "p_brand" => "Brand#23",
        "p_type" => "MEDIUM POLISHED",
        "p_size" => 10
    ]
];
$partsupp = [
    ["ps_partkey" => 1, "ps_suppkey" => 100],
    ["ps_partkey" => 2, "ps_suppkey" => 200]
];
$excluded_suppliers = (function() use ($part, $partsupp) {
    $result = [];
    foreach ($partsupp as $ps) {
        foreach ($part as $p) {
            if ($p['p_partkey'] == $ps['ps_partkey']) {
                if ($p['p_brand'] == "Brand#12" && strpos($p['p_type'], "SMALL") !== false && $p['p_size'] == 5) {
                    $result[] = $ps['ps_suppkey'];
                }
            }
        }
    }
    return $result;
})();
$result = (function() use ($excluded_suppliers, $supplier) {
    $result = [];
    foreach ($supplier as $s) {
        if (!(in_array($s['s_suppkey'], $excluded_suppliers)) && (!strpos($s['s_comment'], "Customer") !== false) && (!strpos($s['s_comment'], "Complaints") !== false)) {
            $result[] = [$s['s_name'], [
    "s_name" => $s['s_name'],
    "s_address" => $s['s_address']
]];
        }
    }
    usort($result, function($a, $b) { return $a[0] <=> $b[0]; });
    $result = array_map(fn($r) => $r[1], $result);
    return $result;
})();
echo json_encode($result), PHP_EOL;
?>
