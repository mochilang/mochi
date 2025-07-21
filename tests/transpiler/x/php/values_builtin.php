<?php
$m = ["a" => 1, "b" => 2, "c" => 3];
echo (is_float(values($m)) ? json_encode(values($m), 1344) : values($m)), PHP_EOL;
