<?php
$nation = [["n_nationkey" => 1, "n_name" => "BRAZIL"]];
$customer = [["c_custkey" => 1, "c_name" => "Alice", "c_acctbal" => 100, "c_nationkey" => 1, "c_address" => "123 St", "c_phone" => "123-456", "c_comment" => "Loyal"]];
$orders = [["o_orderkey" => 1000, "o_custkey" => 1, "o_orderdate" => "1993-10-15"], ["o_orderkey" => 2000, "o_custkey" => 1, "o_orderdate" => "1994-01-02"]];
$lineitem = [["l_orderkey" => 1000, "l_returnflag" => "R", "l_extendedprice" => 1000, "l_discount" => 0.1], ["l_orderkey" => 2000, "l_returnflag" => "N", "l_extendedprice" => 500, "l_discount" => 0]];
$start_date = "1993-10-01";
$end_date = "1994-01-01";
$result = (function() use ($customer, $end_date, $lineitem, $nation, $orders, $start_date) {
    $groups = [];
    foreach ($customer as $c) {
        foreach ($orders as $o) {
            if ($o['o_custkey'] == $c['c_custkey']) {
                foreach ($lineitem as $l) {
                    if ($l['l_orderkey'] == $o['o_orderkey']) {
                        foreach ($nation as $n) {
                            if ($n['n_nationkey'] == $c['c_nationkey']) {
                                if ($o['o_orderdate'] >= $start_date && $o['o_orderdate'] < $end_date && $l['l_returnflag'] == "R") {
                                    $_k = json_encode(["c_custkey" => $c['c_custkey'], "c_name" => $c['c_name'], "c_acctbal" => $c['c_acctbal'], "c_address" => $c['c_address'], "c_phone" => $c['c_phone'], "c_comment" => $c['c_comment'], "n_name" => $n['n_name']]);
                                    $groups[$_k][] = $c;
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    $result = [];
    foreach ($groups as $_k => $__g) {
        $g = ['key'=>json_decode($_k, true),'items'=> $__g];
        $result[] = [-array_sum((function() use ($g) {
    $result = [];
    foreach ($g['items'] as $x) {
        $result[] = $x['l']['l_extendedprice'] * (1 - $x['l']['l_discount']);
    }
    return $result;
})()), ["c_custkey" => $g['key']['c_custkey'], "c_name" => $g['key']['c_name'], "revenue" => array_sum((function() use ($g) {
    $result = [];
    foreach ($g['items'] as $x) {
        $result[] = $x['l']['l_extendedprice'] * (1 - $x['l']['l_discount']);
    }
    return $result;
})()), "c_acctbal" => $g['key']['c_acctbal'], "n_name" => $g['key']['n_name'], "c_address" => $g['key']['c_address'], "c_phone" => $g['key']['c_phone'], "c_comment" => $g['key']['c_comment']]];
    }
    usort($result, function($a, $b) { return $a[0] <=> $b[0]; });
    $result = array_map(fn($r) => $r[1], $result);
    return $result;
})();
var_dump($result);
?>
