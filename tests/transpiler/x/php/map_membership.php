<?php
$m = ["a" => 1, "b" => 2];
echo rtrim((array_key_exists("a", $m) ? "true" : "false")), PHP_EOL;
echo rtrim((array_key_exists("c", $m) ? "true" : "false")), PHP_EOL;
?>
