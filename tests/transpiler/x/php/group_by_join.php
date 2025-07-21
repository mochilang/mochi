<?php
$customers = [["id" => 1, "name" => "Alice"], ["id" => 2, "name" => "Bob"]];
$orders = [["id" => 100, "customerId" => 1], ["id" => 101, "customerId" => 1], ["id" => 102, "customerId" => 2]];
$stats = (function() use ($customers, $orders) {
  $groups = [];
  foreach ($orders as $o) {
    foreach ($customers as $c) {
      if ($o["customerId"] == $c["id"]) {
        $key = $c["name"];
        if (!array_key_exists($key, $groups)) {
          $groups[$key] = ['key' => $key, 'items' => []];
        }
        $groups[$key]['items'][] = ['o' => $o, 'c' => $c];
      }
    }
  }
  $result = [];
  foreach ($groups as $g) {
      $result[] = ["name" => $g["key"], "count" => count($g["items"])];
  }
  return $result;
})();
echo rtrim("--- Orders per customer ---"), PHP_EOL;
foreach ($stats as $s) {
  echo rtrim($s["name"] . " " . "orders:" . " " . $s["count"]), PHP_EOL;
}
?>
