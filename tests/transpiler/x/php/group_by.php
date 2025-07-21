<?php
$people = [["name" => "Alice", "age" => 30, "city" => "Paris"], ["name" => "Bob", "age" => 15, "city" => "Hanoi"], ["name" => "Charlie", "age" => 65, "city" => "Paris"], ["name" => "Diana", "age" => 45, "city" => "Hanoi"], ["name" => "Eve", "age" => 70, "city" => "Paris"], ["name" => "Frank", "age" => 22, "city" => "Hanoi"]];
$stats = (function() use ($people) {
  $groups = [];
  foreach ($people as $person) {
    $key = $person["city"];
    if (!array_key_exists($key, $groups)) {
      $groups[$key] = ['key' => $key, 'items' => []];
    }
    $groups[$key]['items'][] = $person;
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
  echo $s["city"] . " " . ": count =" . " " . $s["count"] . " " . ", avg_age =" . " " . $s["avg_age"], PHP_EOL;
}
?>
