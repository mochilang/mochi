<?php
$customers = [["id" => 1, "name" => "Alice"], ["id" => 2, "name" => "Bob"], ["id" => 3, "name" => "Charlie"], ["id" => 4, "name" => "Diana"]];
$orders = [["id" => 100, "customerId" => 1, "total" => 250], ["id" => 101, "customerId" => 2, "total" => 125], ["id" => 102, "customerId" => 1, "total" => 300]];
$result = (function() use ($customers, $orders) {
  $result = [];
  foreach ($orders as $o) {
    $matched = false;
    foreach ($customers as $c) {
      if (!($o["customerId"] == $c["id"])) continue;
      $matched = true;
      $result[] = ["customerName" => $c["name"], "order" => $o];
    }
    if (!$matched) {
      $c = null;
      $result[] = ["customerName" => $c["name"], "order" => $o];
    }
  }
  return $result;
})();
echo "--- Right Join using syntax ---", PHP_EOL;
foreach ($result as $entry) {
  if ($entry["order"]) {
  echo "Customer" . " " . $entry["customerName"] . " " . "has order" . " " . $entry["order"]["id"] . " " . "- $" . " " . $entry["order"]["total"], PHP_EOL;
} else {
  echo "Customer" . " " . $entry["customerName"] . " " . "has no orders", PHP_EOL;
}
}
?>
