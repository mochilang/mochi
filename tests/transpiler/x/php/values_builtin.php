<?php
$m = ["a" => 1, "b" => 2, "c" => 3];
echo rtrim((is_float(values($m)) ? sprintf("%.15f", values($m)) : values($m))), PHP_EOL;
?>
