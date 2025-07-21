<?php
$m = ["a" => 1, "b" => 2, "c" => 3];
echo str_replace("false", "False", str_replace("true", "True", str_replace("\"", "'", str_replace(":", ": ", str_replace(",", ", ", json_encode(array_values($m), 1344)))))), PHP_EOL;
