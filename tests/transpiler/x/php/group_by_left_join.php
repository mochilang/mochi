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
      $k = json_encode($key);
      if (!array_key_exists($k, $groups)) {
        $groups[$k] = ['key' => $key, 'items' => []];
      }
      $groups[$k]['items'][] = ['c' => $c, 'o' => $o];
    }
    if (!$matched) {
      $o = null;
      $key = $c["name"];
      $k = json_encode($key);
      if (!array_key_exists($k, $groups)) {
        $groups[$k] = ['key' => $key, 'items' => []];
      }
      $groups[$k]['items'][] = ['c' => $c, 'o' => $o];
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
echo "--- Group Left Join ---", PHP_EOL;
foreach ($stats as $s) {
  echo (is_float($s["name"]) ? json_encode($s["name"], 1344) : $s["name"]) . " " . "orders:" . " " . (is_float($s["count"]) ? json_encode($s["count"], 1344) : $s["count"]), PHP_EOL;
}
