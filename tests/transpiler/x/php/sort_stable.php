<?php
$items = [["n" => 1, "v" => "a"], ["n" => 1, "v" => "b"], ["n" => 2, "v" => "c"]];
$result = [];
foreach ($items as $i) {
  $result[] = $i["v"];
}

echo rtrim(str_replace(":", ": ", str_replace(",", ", ", json_encode($result, 320)))), PHP_EOL;
?>
