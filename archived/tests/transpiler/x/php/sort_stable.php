<?php
$items = [["n" => 1, "v" => "a"], ["n" => 1, "v" => "b"], ["n" => 2, "v" => "c"]];
$result = [];
foreach ($items as $i) {
  $result[] = $i["v"];
}

echo str_replace("false", "False", str_replace("true", "True", str_replace("\"", "'", str_replace(":", ": ", str_replace(",", ", ", json_encode($result, 1344)))))), PHP_EOL;
