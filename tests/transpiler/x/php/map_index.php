<?php
$m = ["a" => 1, "b" => 2];
echo (is_float($m["b"]) ? json_encode($m["b"], 1344) : $m["b"]), PHP_EOL;
