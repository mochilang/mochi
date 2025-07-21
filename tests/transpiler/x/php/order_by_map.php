<?php
$data = [["a" => 1, "b" => 2], ["a" => 1, "b" => 1], ["a" => 0, "b" => 5]];
$sorted = [];
foreach ($data as $x) {
  $sorted[] = $x;
}

echo rtrim(implode(" ", array_map("json_encode", $sorted))), PHP_EOL;
?>
