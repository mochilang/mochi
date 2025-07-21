<?php
$data = ["outer" => ["inner" => 1]];
$data["outer"]["inner"] = 2;
echo rtrim((is_float($data["outer"]["inner"]) ? sprintf("%.15f", $data["outer"]["inner"]) : $data["outer"]["inner"])), PHP_EOL;
?>
