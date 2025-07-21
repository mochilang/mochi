<?php
$data = [["a" => 1, "b" => 2], ["a" => 1, "b" => 1], ["a" => 0, "b" => 5]];
$sorted = [];
foreach ($data as $x) {
  $sorted[] = $x;
}

echo str_replace("false", "False", str_replace("true", "True", str_replace("\"", "'", str_replace(":", ": ", str_replace(",", ", ", json_encode($sorted, 1344)))))), PHP_EOL;
