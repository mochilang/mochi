<?php
$data = ["outer" => ["inner" => 1]];
$data["outer"]["inner"] = 2;
echo $data["outer"]["inner"], PHP_EOL;
?>
