<?php
function mochi_test_Q1_aggregates_revenue_and_quantity_by_returnflag___linestatus() {
	global $result;
	if (!(($result == [["returnflag" => "N", "linestatus" => "O", "sum_qty" => 53, "sum_base_price" => 3000, "sum_disc_price" => ((is_array(950.0) && is_array(1800.0)) ? array_merge(950.0, 1800.0) : ((is_string(950.0) || is_string(1800.0)) ? (950.0 . 1800.0) : (950.0 + 1800.0))), "sum_charge" => ((is_array(((950.0 * 1.07))) && is_array(((1800.0 * 1.05)))) ? array_merge(((950.0 * 1.07)), ((1800.0 * 1.05))) : ((is_string(((950.0 * 1.07))) || is_string(((1800.0 * 1.05)))) ? (((950.0 * 1.07)) . ((1800.0 * 1.05))) : (((950.0 * 1.07)) + ((1800.0 * 1.05))))), "avg_qty" => 26.5, "avg_price" => 1500, "avg_disc" => 0.07500000000000001, "count_order" => 2]]))) { throw new Exception("expect failed: ($result == [['returnflag' => 'N', 'linestatus' => 'O', 'sum_qty' => 53, 'sum_base_price' => 3000, 'sum_disc_price' => ((is_array(950.0) && is_array(1800.0)) ? array_merge(950.0, 1800.0) : ((is_string(950.0) || is_string(1800.0)) ? (950.0 . 1800.0) : (950.0 + 1800.0))), 'sum_charge' => ((is_array(((950.0 * 1.07))) && is_array(((1800.0 * 1.05)))) ? array_merge(((950.0 * 1.07)), ((1800.0 * 1.05))) : ((is_string(((950.0 * 1.07))) || is_string(((1800.0 * 1.05)))) ? (((950.0 * 1.07)) . ((1800.0 * 1.05))) : (((950.0 * 1.07)) + ((1800.0 * 1.05))))), 'avg_qty' => 26.5, 'avg_price' => 1500, 'avg_disc' => 0.07500000000000001, 'count_order' => 2]])"); }
}

$lineitem = [["l_quantity" => 17, "l_extendedprice" => 1000.0, "l_discount" => 0.05, "l_tax" => 0.07, "l_returnflag" => "N", "l_linestatus" => "O", "l_shipdate" => "1998-08-01"], ["l_quantity" => 36, "l_extendedprice" => 2000.0, "l_discount" => 0.1, "l_tax" => 0.05, "l_returnflag" => "N", "l_linestatus" => "O", "l_shipdate" => "1998-09-01"], ["l_quantity" => 25, "l_extendedprice" => 1500.0, "l_discount" => 0.0, "l_tax" => 0.08, "l_returnflag" => "R", "l_linestatus" => "F", "l_shipdate" => "1998-09-03"]];
$result = (function() use ($lineitem) {
	$_src = (is_string($lineitem) ? str_split($lineitem) : $lineitem);
	$_src = array_values(array_filter($_src, function($row) use ($lineitem) { return (($row['l_shipdate'] <= "1998-09-02")); }));
	$_groups = _group_by($_src, function($row) use ($lineitem) { return ["returnflag" => $row['l_returnflag'], "linestatus" => $row['l_linestatus']]; });
	$res = [];
	foreach ($_groups as $g) {
		$res[] = ["returnflag" => $g->key->returnflag, "linestatus" => $g->key->linestatus, "sum_qty" => array_sum((function() use ($g) {
	$res = [];
	foreach ((is_string($g->Items) ? str_split($g->Items) : $g->Items) as $x) {
		$res[] = $x['l_quantity'];
	}
	return $res;
})()), "sum_base_price" => array_sum((function() use ($g) {
	$res = [];
	foreach ((is_string($g->Items) ? str_split($g->Items) : $g->Items) as $x) {
		$res[] = $x['l_extendedprice'];
	}
	return $res;
})()), "sum_disc_price" => array_sum((function() use ($g) {
	$res = [];
	foreach ((is_string($g->Items) ? str_split($g->Items) : $g->Items) as $x) {
		$res[] = ($x['l_extendedprice'] * ((1 - $x['l_discount'])));
	}
	return $res;
})()), "sum_charge" => array_sum((function() use ($g) {
	$res = [];
	foreach ((is_string($g->Items) ? str_split($g->Items) : $g->Items) as $x) {
		$res[] = (($x['l_extendedprice'] * ((1 - $x['l_discount']))) * (((is_array(1) && is_array($x['l_tax'])) ? array_merge(1, $x['l_tax']) : ((is_string(1) || is_string($x['l_tax'])) ? (1 . $x['l_tax']) : (1 + $x['l_tax'])))));
	}
	return $res;
})()), "avg_qty" => (count((function() use ($g) {
	$res = [];
	foreach ((is_string($g->Items) ? str_split($g->Items) : $g->Items) as $x) {
		$res[] = $x['l_quantity'];
	}
	return $res;
})()) ? array_sum((function() use ($g) {
	$res = [];
	foreach ((is_string($g->Items) ? str_split($g->Items) : $g->Items) as $x) {
		$res[] = $x['l_quantity'];
	}
	return $res;
})()) / count((function() use ($g) {
	$res = [];
	foreach ((is_string($g->Items) ? str_split($g->Items) : $g->Items) as $x) {
		$res[] = $x['l_quantity'];
	}
	return $res;
})()) : 0), "avg_price" => (count((function() use ($g) {
	$res = [];
	foreach ((is_string($g->Items) ? str_split($g->Items) : $g->Items) as $x) {
		$res[] = $x['l_extendedprice'];
	}
	return $res;
})()) ? array_sum((function() use ($g) {
	$res = [];
	foreach ((is_string($g->Items) ? str_split($g->Items) : $g->Items) as $x) {
		$res[] = $x['l_extendedprice'];
	}
	return $res;
})()) / count((function() use ($g) {
	$res = [];
	foreach ((is_string($g->Items) ? str_split($g->Items) : $g->Items) as $x) {
		$res[] = $x['l_extendedprice'];
	}
	return $res;
})()) : 0), "avg_disc" => (count((function() use ($g) {
	$res = [];
	foreach ((is_string($g->Items) ? str_split($g->Items) : $g->Items) as $x) {
		$res[] = $x['l_discount'];
	}
	return $res;
})()) ? array_sum((function() use ($g) {
	$res = [];
	foreach ((is_string($g->Items) ? str_split($g->Items) : $g->Items) as $x) {
		$res[] = $x['l_discount'];
	}
	return $res;
})()) / count((function() use ($g) {
	$res = [];
	foreach ((is_string($g->Items) ? str_split($g->Items) : $g->Items) as $x) {
		$res[] = $x['l_discount'];
	}
	return $res;
})()) : 0), "count_order" => (is_array($g->Items) ? count($g->Items) : strlen($g->Items))];
	}
	return $res;
})();
echo json_encode($result), PHP_EOL;
mochi_test_Q1_aggregates_revenue_and_quantity_by_returnflag___linestatus();

class _Group {
    public $key;
    public $Items;
    function __construct($key) { $this->key = $key; $this->Items = []; }
}
function _group_by($src, $keyfn) {
    $groups = [];
    $order = [];
    foreach ($src as $it) {
        $key = $keyfn($it);
        if (is_array($key)) { $key = (object)$key; }
        $ks = is_object($key) ? json_encode($key) : strval($key);
        if (!isset($groups[$ks])) {
            $groups[$ks] = new _Group($key);
            $order[] = $ks;
        }
        $groups[$ks]->Items[] = $it;
    }
    $res = [];
    foreach ($order as $ks) { $res[] = $groups[$ks]; }
    return $res;
}
