<?php
$x = 3;
$y = 4;
$m = ["a" => $x, "b" => $y];
echo  . (is_float($m["b"]) ? json_encode($m["b"], 1344) : $m["b"]), PHP_EOL;
