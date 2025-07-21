<?php
$x = 3;
$y = 4;
$m = ["a" => $x, "b" => $y];
echo rtrim((is_float($m["a"]) ? sprintf("%.15f", $m["a"]) : $m["a"]) . " " . (is_float($m["b"]) ? sprintf("%.15f", $m["b"]) : $m["b"])), PHP_EOL;
?>
