<?php
$customers = [["id" => 1, "name" => "Alice"], ["id" => 2, "name" => "Bob"]];
$orders = [["id" => 100, "customerId" => 1], ["id" => 101, "customerId" => 1], ["id" => 102, "customerId" => 2]];
$stats = (function() use ($customers, $orders) {
  $groups = [];
  foreach ($orders as $o) {
    foreach ($customers as $c) {
      if ($o["customerId"] == $c["id"]) {
        $key = $c["name"];
        $k = json_encode($key);
        if (!array_key_exists($k, $groups)) {
          $groups[$k] = ['key' => $key, 'items' => []];
        }
        $groups[$k]['items'][] = ['o' => $o, 'c' => $c];
      }
    }
  }
  $result = [];
  foreach ($groups as $g) {
      $result[] = ["name" => $g["key"], "count" => count($g["items"])];
  }
  return $result;
})();
echo "--- Orders per customer ---", PHP_EOL;
foreach ($stats as $s) {
  echo (is_float($s["name"]) ? json_encode($s["name"], 1344) : $s["name"]) . " " . "orders:" . " " . (is_float($s["count"]) ? json_encode($s["count"], 1344) : $s["count"]), PHP_EOL;
}
