<?php
$data = ["outer" => ["inner" => 1]];
$data["outer"]["inner"] = 2;
echo (is_float($data["outer"]["inner"]) ? json_encode($data["outer"]["inner"], 1344) : $data["outer"]["inner"]), PHP_EOL;
