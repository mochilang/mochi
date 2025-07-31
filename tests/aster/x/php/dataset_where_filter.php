<?php
$people = [["name" => "Alice", "age" => 30], ["name" => "Bob", "age" => 15], ["name" => "Charlie", "age" => 65], ["name" => "Diana", "age" => 45]];
;
foreach ($people as $person) {
    if ($person["age"] >= 18) {
         = ["name" => $person["name"], "age" => $person["age"], "is_senior" => $person["age"] >= 60];
    }
}
echo "--- Adults ---", PHP_EOL;
foreach ($adults as $person) {
    echo  . ($person["is_senior"] ? " (senior)" : "\"\""), PHP_EOL;
}
