<?php
$people = [["name" => "Alice", "city" => "Paris"], ["name" => "Bob", "city" => "Hanoi"], ["name" => "Charlie", "city" => "Paris"], ["name" => "Diana", "city" => "Hanoi"], ["name" => "Eve", "city" => "Paris"], ["name" => "Frank", "city" => "Hanoi"], ["name" => "George", "city" => "Paris"]];
$big = (function() use ($people) {
  $groups = [];
  foreach ($people as $p) {
    $key = $p["city"];
    $k = json_encode($key);
    if (!array_key_exists($k, $groups)) {
      $groups[$k] = ['key' => $key, 'items' => []];
    }
    $groups[$k]['items'][] = $p;
  }
  $result = [];
  foreach ($groups as $g) {
    if (count($g["items"]) >= 4) {
      $result[] = ["city" => $g["key"], "num" => count($g["items"])];
    }
  }
  return $result;
})();
echo str_replace("    ", "  ", json_encode($big, 128)), PHP_EOL;
