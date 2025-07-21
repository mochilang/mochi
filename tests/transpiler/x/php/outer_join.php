<?php
$customers = [["id" => 1, "name" => "Alice"], ["id" => 2, "name" => "Bob"], ["id" => 3, "name" => "Charlie"], ["id" => 4, "name" => "Diana"]];
$orders = [["id" => 100, "customerId" => 1, "total" => 250], ["id" => 101, "customerId" => 2, "total" => 125], ["id" => 102, "customerId" => 1, "total" => 300], ["id" => 103, "customerId" => 5, "total" => 80]];
$result = (function() use ($customers, $orders) {
  $result = [];
  foreach ($orders as $o) {
    $matched = false;
    foreach ($customers as $c) {
      if (!($o["customerId"] == $c["id"])) continue;
      $matched = true;
      $result[] = ["order" => $o, "customer" => $c];
    }
    if (!$matched) {
      $c = null;
      $result[] = ["order" => $o, "customer" => $c];
    }
  }
  foreach ($customers as $c) {
    $matched = false;
    foreach ($orders as $o) {
      if (!($o["customerId"] == $c["id"])) continue;
      $matched = true;
      break;
    }
    if (!$matched) {
      $o = null;
      $result[] = ["order" => $o, "customer" => $c];
    }
  }
  return $result;
})();
echo rtrim("--- Outer Join using syntax ---"), PHP_EOL;
foreach ($result as $row) {
  if ($row["order"]) {
  if ($row["customer"]) {
  echo rtrim("Order" . " " . (is_float($row["order"]["id"]) ? sprintf("%.15f", $row["order"]["id"]) : $row["order"]["id"]) . " " . "by" . " " . (is_float($row["customer"]["name"]) ? sprintf("%.15f", $row["customer"]["name"]) : $row["customer"]["name"]) . " " . "- $" . " " . (is_float($row["order"]["total"]) ? sprintf("%.15f", $row["order"]["total"]) : $row["order"]["total"])), PHP_EOL;
} else {
  echo rtrim("Order" . " " . (is_float($row["order"]["id"]) ? sprintf("%.15f", $row["order"]["id"]) : $row["order"]["id"]) . " " . "by" . " " . "Unknown" . " " . "- $" . " " . (is_float($row["order"]["total"]) ? sprintf("%.15f", $row["order"]["total"]) : $row["order"]["total"])), PHP_EOL;
};
} else {
  echo rtrim("Customer" . " " . (is_float($row["customer"]["name"]) ? sprintf("%.15f", $row["customer"]["name"]) : $row["customer"]["name"]) . " " . "has no orders"), PHP_EOL;
}
}
?>
