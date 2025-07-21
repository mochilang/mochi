<?php
$customers = [["id" => 1, "name" => "Alice"], ["id" => 2, "name" => "Bob"], ["id" => 3, "name" => "Charlie"]];
$orders = [["id" => 100, "customerId" => 1], ["id" => 101, "customerId" => 1], ["id" => 102, "customerId" => 2]];
$stats = (function() use ($customers, $orders) {
  $groups = [];
  foreach ($customers as $c) {
    $matched = false;
    foreach ($orders as $o) {
      if (!($o["customerId"] == $c["id"])) continue;
      $matched = true;
      $key = $c["name"];
      if (!array_key_exists($key, $groups)) {
        $groups[$key] = ['key' => $key, 'items' => []];
      }
      $groups[$key]['items'][] = ['c' => $c, 'o' => $o];
    }
    if (!$matched) {
      $o = null;
      $key = $c["name"];
      if (!array_key_exists($key, $groups)) {
        $groups[$key] = ['key' => $key, 'items' => []];
      }
      $groups[$key]['items'][] = ['c' => $c, 'o' => $o];
    }
  }
  $result = [];
  foreach ($groups as $g) {
      $result[] = ["name" => $g["key"], "count" => count((function() use ($c, $o, $g) {
  $result = [];
  foreach ($g["items"] as $r) {
    if ($r["o"]) {
      $result[] = $r;
    }
  }
  return $result;
})())];
  }
  return $result;
})();
echo rtrim("--- Group Left Join ---"), PHP_EOL;
foreach ($stats as $s) {
  echo rtrim((is_float($s["name"]) ? sprintf("%.15f", $s["name"]) : $s["name"]) . " " . "orders:" . " " . (is_float($s["count"]) ? sprintf("%.15f", $s["count"]) : $s["count"])), PHP_EOL;
}
?>
