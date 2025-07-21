<?php
$scores = ["alice" => 1];
$scores["bob"] = 2;
echo rtrim((is_float($scores["bob"]) ? sprintf("%.15f", $scores["bob"]) : $scores["bob"])), PHP_EOL;
?>
