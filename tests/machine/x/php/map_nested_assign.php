<?php
$data = ["outer" => ["inner" => 1]];
$data["outer"]["inner"] = 2;
var_dump($data["outer"]["inner"]);
?>
