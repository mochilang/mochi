<?php
$people = [["name" => "Alice", "age" => 30, "city" => "Paris"], ["name" => "Bob", "age" => 15, "city" => "Hanoi"], ["name" => "Charlie", "age" => 65, "city" => "Paris"], ["name" => "Diana", "age" => 45, "city" => "Hanoi"], ["name" => "Eve", "age" => 70, "city" => "Paris"], ["name" => "Frank", "age" => 22, "city" => "Hanoi"]];
$stats = (function() use ($people) {
  $groups = [];
  foreach ($people as $person) {
    $key = $person["city"];
    $k = json_encode($key);
    if (!array_key_exists($k, $groups)) {
      $groups[$k] = ['key' => $key, 'items' => []];
    }
    $groups[$k]['items'][] = $person;
  }
  $result = [];
  foreach ($groups as $g) {
      $result[] = ["city" => $g["key"], "count" => count($g["items"]), "avg_age" => array_sum((function() use ($person, $g) {
  $result = [];
  foreach ($g["items"] as $p) {
    $result[] = $p["age"];
  }
  return $result;
})()) / count((function() use ($person, $g) {
  $result = [];
  foreach ($g["items"] as $p) {
    $result[] = $p["age"];
  }
  return $result;
})())];
  }
  return $result;
})();
echo "--- People grouped by city ---", PHP_EOL;
foreach ($stats as $s) {
  echo (is_float($s["city"]) ? json_encode($s["city"], 1344) : $s["city"]) . " " . ": count =" . " " . (is_float($s["count"]) ? json_encode($s["count"], 1344) : $s["count"]) . " " . ", avg_age =" . " " . (is_float($s["avg_age"]) ? json_encode($s["avg_age"], 1344) : $s["avg_age"]), PHP_EOL;
}
