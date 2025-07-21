<?php
$data = [["a" => 1, "b" => 2], ["a" => 1, "b" => 1], ["a" => 0, "b" => 5]];
$sorted = [];
foreach ($data as $x) {
  $sorted[] = $x;
}

echo rtrim(str_replace(":", ": ", str_replace(",", ", ", json_encode($sorted, 320)))), PHP_EOL;
?>
