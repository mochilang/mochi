<?php
$region = [
    [
        "r_regionkey" => 1,
        "r_name" => "EUROPE"
    ],
    ["r_regionkey" => 2, "r_name" => "ASIA"]
];
$nation = [
    [
        "n_nationkey" => 10,
        "n_regionkey" => 1,
        "n_name" => "FRANCE"
    ],
    [
        "n_nationkey" => 20,
        "n_regionkey" => 2,
        "n_name" => "CHINA"
    ]
];
$supplier = [
    [
        "s_suppkey" => 100,
        "s_name" => "BestSupplier",
        "s_address" => "123 Rue",
        "s_nationkey" => 10,
        "s_phone" => "123",
        "s_acctbal" => 1000,
        "s_comment" => "Fast and reliable"
    ],
    [
        "s_suppkey" => 200,
        "s_name" => "AltSupplier",
        "s_address" => "456 Way",
        "s_nationkey" => 20,
        "s_phone" => "456",
        "s_acctbal" => 500,
        "s_comment" => "Slow"
    ]
];
$part = [
    [
        "p_partkey" => 1000,
        "p_type" => "LARGE BRASS",
        "p_size" => 15,
        "p_mfgr" => "M1"
    ],
    [
        "p_partkey" => 2000,
        "p_type" => "SMALL COPPER",
        "p_size" => 15,
        "p_mfgr" => "M2"
    ]
];
$partsupp = [
    [
        "ps_partkey" => 1000,
        "ps_suppkey" => 100,
        "ps_supplycost" => 10
    ],
    [
        "ps_partkey" => 1000,
        "ps_suppkey" => 200,
        "ps_supplycost" => 15
    ]
];
$europe_nations = (function() use ($nation, $region) {
    $result = [];
    foreach ($region as $r) {
        foreach ($nation as $n) {
            if ($n['n_regionkey'] == $r['r_regionkey']) {
                if ($r['r_name'] == "EUROPE") {
                    $result[] = $n;
                }
            }
        }
    }
    return $result;
})();
$europe_suppliers = (function() use ($europe_nations, $supplier) {
    $result = [];
    foreach ($supplier as $s) {
        foreach ($europe_nations as $n) {
            if ($s['s_nationkey'] == $n['n_nationkey']) {
                $result[] = ["s" => $s, "n" => $n];
            }
        }
    }
    return $result;
})();
$target_parts = (function() use ($part) {
    $result = [];
    foreach ($part as $p) {
        if ($p['p_size'] == 15 && $p['p_type'] == "LARGE BRASS") {
            $result[] = $p;
        }
    }
    return $result;
})();
$target_partsupp = (function() use ($europe_suppliers, $partsupp, $target_parts) {
    $result = [];
    foreach ($partsupp as $ps) {
        foreach ($target_parts as $p) {
            if ($ps['ps_partkey'] == $p['p_partkey']) {
                foreach ($europe_suppliers as $s) {
                    if ($ps['ps_suppkey'] == $s['s']['s_suppkey']) {
                        $result[] = [
    "s_acctbal" => $s['s']['s_acctbal'],
    "s_name" => $s['s']['s_name'],
    "n_name" => $s['n']['n_name'],
    "p_partkey" => $p['p_partkey'],
    "p_mfgr" => $p['p_mfgr'],
    "s_address" => $s['s']['s_address'],
    "s_phone" => $s['s']['s_phone'],
    "s_comment" => $s['s']['s_comment'],
    "ps_supplycost" => $ps['ps_supplycost']
];
                    }
                }
            }
        }
    }
    return $result;
})();
$costs = (function() use ($target_partsupp) {
    $result = [];
    foreach ($target_partsupp as $x) {
        $result[] = $x['ps_supplycost'];
    }
    return $result;
})();
$min_cost = min($costs);
$result = (function() use ($min_cost, $target_partsupp) {
    $result = [];
    foreach ($target_partsupp as $x) {
        if ($x['ps_supplycost'] == $min_cost) {
            $result[] = [-$x['s_acctbal'], $x];
        }
    }
    usort($result, function($a, $b) { return $a[0] <=> $b[0]; });
    $result = array_map(fn($r) => $r[1], $result);
    return $result;
})();
echo json_encode($result), PHP_EOL;
?>
