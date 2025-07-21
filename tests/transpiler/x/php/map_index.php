<?php
$m = ["a" => 1, "b" => 2];
echo rtrim((is_float($m["b"]) ? sprintf("%.15f", $m["b"]) : $m["b"])), PHP_EOL;
?>
