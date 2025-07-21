<?php
$scores = ["alice" => 1];
$scores["bob"] = 2;
echo (is_float($scores["bob"]) ? json_encode($scores["bob"], 1344) : $scores["bob"]), PHP_EOL;
