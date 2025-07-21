<?php
$items = [["n" => 1, "v" => "a"], ["n" => 1, "v" => "b"], ["n" => 2, "v" => "c"]];
$result = [];
foreach ($items as $i) {
  $result[] = $i["v"];
}

echo rtrim(implode(" ", array_map("json_encode", $result))), PHP_EOL;
?>
