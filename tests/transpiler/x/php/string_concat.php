<?php
echo rtrim((is_float("hello " . "world") ? sprintf("%.15f", "hello " . "world") : "hello " . "world")), PHP_EOL;
?>
