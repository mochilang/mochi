<?php
$m = ["a" => 1, "b" => 2];
echo rtrim((is_float((array_key_exists("a", $m) ? 1 : 0)) ? sprintf("%.15f", (array_key_exists("a", $m) ? 1 : 0)) : (array_key_exists("a", $m) ? 1 : 0))), PHP_EOL;
echo rtrim((is_float((array_key_exists("c", $m) ? 1 : 0)) ? sprintf("%.15f", (array_key_exists("c", $m) ? 1 : 0)) : (array_key_exists("c", $m) ? 1 : 0))), PHP_EOL;
?>
