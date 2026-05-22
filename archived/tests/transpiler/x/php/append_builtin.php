<?php
$a = [1, 2];
echo str_replace("false", "False", str_replace("true", "True", str_replace("\"", "'", str_replace(":", ": ", str_replace(",", ", ", json_encode(array_merge($a, [3]), 1344)))))), PHP_EOL;
