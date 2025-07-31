<?php
$m = [1 => "a", 2 => "b"];
echo (is_float($m[1]) ? json_encode($m[1], 1344) : $m[1]), PHP_EOL;
