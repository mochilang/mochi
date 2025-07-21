<?php
$m = [1 => "a", 2 => "b"];
echo rtrim((array_key_exists(1, $m) ? "true" : "false")), PHP_EOL;
echo rtrim((array_key_exists(3, $m) ? "true" : "false")), PHP_EOL;
?>
