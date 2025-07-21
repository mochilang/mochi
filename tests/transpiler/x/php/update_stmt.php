<?php
$people = [["name" => "Alice", "age" => 17, "status" => "minor"], ["name" => "Bob", "age" => 25, "status" => "unknown"], ["name" => "Charlie", "age" => 18, "status" => "unknown"], ["name" => "Diana", "age" => 16, "status" => "minor"]];
foreach ($people as $idx => $item) {
  if ($item["age"] >= 18) {
    $item['status'] = "adult";
    $item['age'] = $item["age"] + 1;
  }
  $people[$idx] = $item;
};
echo "ok", PHP_EOL;
