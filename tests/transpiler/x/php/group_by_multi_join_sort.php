<?php
$nation = [["n_nationkey" => 1, "n_name" => "BRAZIL"]];
$customer = [["c_custkey" => 1, "c_name" => "Alice", "c_acctbal" => 100.0, "c_nationkey" => 1, "c_address" => "123 St", "c_phone" => "123-456", "c_comment" => "Loyal"]];
$orders = [["o_orderkey" => 1000, "o_custkey" => 1, "o_orderdate" => "1993-10-15"], ["o_orderkey" => 2000, "o_custkey" => 1, "o_orderdate" => "1994-01-02"]];
$lineitem = [["l_orderkey" => 1000, "l_returnflag" => "R", "l_extendedprice" => 1000.0, "l_discount" => 0.1], ["l_orderkey" => 2000, "l_returnflag" => "N", "l_extendedprice" => 500.0, "l_discount" => 0.0]];
$start_date = "1993-10-01";
$end_date = "1994-01-01";
$result = (function() use ($nation, $customer, $orders, $lineitem, $start_date, $end_date) {
  $groups = [];
  foreach ($customer as $c) {
    foreach ($orders as $o) {
      foreach ($lineitem as $l) {
        foreach ($nation as $n) {
          if ($o["o_custkey"] == $c["c_custkey"] && $l["l_orderkey"] == $o["o_orderkey"] && $n["n_nationkey"] == $c["c_nationkey"] && $o["o_orderdate"] >= $start_date && $o["o_orderdate"] < $end_date && $l["l_returnflag"] == "R") {
            $key = ["c_custkey" => $c["c_custkey"], "c_name" => $c["c_name"], "c_acctbal" => $c["c_acctbal"], "c_address" => $c["c_address"], "c_phone" => $c["c_phone"], "c_comment" => $c["c_comment"], "n_name" => $n["n_name"]];
            if (!array_key_exists($key, $groups)) {
              $groups[$key] = ['key' => $key, 'items' => []];
            }
            $groups[$key]['items'][] = ['c' => $c, 'o' => $o, 'l' => $l, 'n' => $n];
          }
        }
      }
    }
  }
  $result = [];
  foreach ($groups as $g) {
      $result[] = [-array_sum((function() use ($c, $o, $l, $n, $g) {
  $result = [];
  foreach ($g["items"] as $x) {
    $result[] = $x["l"]["l_extendedprice"] * (1 - $x["l"]["l_discount"]);
  }
  return $result;
})()), ["c_custkey" => $g["key"]["c_custkey"], "c_name" => $g["key"]["c_name"], "revenue" => array_sum((function() use ($c, $o, $l, $n, $g) {
  $result = [];
  foreach ($g["items"] as $x) {
    $result[] = $x["l"]["l_extendedprice"] * (1 - $x["l"]["l_discount"]);
  }
  return $result;
})()), "c_acctbal" => $g["key"]["c_acctbal"], "n_name" => $g["key"]["n_name"], "c_address" => $g["key"]["c_address"], "c_phone" => $g["key"]["c_phone"], "c_comment" => $g["key"]["c_comment"]]];
  }
  usort($result, function($a, $b) { return $a[0] <=> $b[0]; });
  $result = array_map(fn($r) => $r[1], $result);
  return $result;
})();
echo rtrim(str_replace("\"", "'", str_replace(":", ": ", str_replace(",", ", ", json_encode($result, 1344))))), PHP_EOL;
?>
