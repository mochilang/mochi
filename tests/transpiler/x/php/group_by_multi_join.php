<?php
$nations = [["id" => 1, "name" => "A"], ["id" => 2, "name" => "B"]];
$suppliers = [["id" => 1, "nation" => 1], ["id" => 2, "nation" => 2]];
$partsupp = [["part" => 100, "supplier" => 1, "cost" => 10.0, "qty" => 2], ["part" => 100, "supplier" => 2, "cost" => 20.0, "qty" => 1], ["part" => 200, "supplier" => 1, "cost" => 5.0, "qty" => 3]];
$filtered = [];
foreach ($partsupp as $ps) {
  foreach ($suppliers as $s) {
    foreach ($nations as $n) {
      if ($s["id"] == $ps["supplier"] && $n["id"] == $s["nation"] && $n["name"] == "A") {
        $filtered[] = ["part" => $ps["part"], "value" => $ps["cost"] * $ps["qty"]];
      }
    }
  }
}

$grouped = (function() use ($nations, $suppliers, $partsupp, $filtered) {
  $groups = [];
  foreach ($filtered as $x) {
    $key = $x["part"];
    if (!array_key_exists($key, $groups)) {
      $groups[$key] = ['key' => $key, 'items' => []];
    }
    $groups[$key]['items'][] = $x;
  }
  $result = [];
  foreach ($groups as $g) {
      $result[] = ["part" => $g["key"], "total" => array_sum((function() use ($x, $g) {
  $result = [];
  foreach ($g["items"] as $r) {
    $result[] = $r["value"];
  }
  return $result;
})())];
  }
  return $result;
})();
echo rtrim(str_replace("\"", "'", str_replace(":", ": ", str_replace(",", ", ", json_encode($grouped, 1344))))), PHP_EOL;
?>
