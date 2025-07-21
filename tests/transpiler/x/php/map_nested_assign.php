<?php
$data = ["outer" => ["inner" => 1]];
$data["outer"]["inner"] = 2;
echo rtrim($data["outer"]["inner"]), PHP_EOL;
?>
