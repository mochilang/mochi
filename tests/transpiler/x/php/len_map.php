<?php
echo rtrim((is_float(count(["a" => 1, "b" => 2])) ? sprintf("%.15f", count(["a" => 1, "b" => 2])) : count(["a" => 1, "b" => 2]))), PHP_EOL;
?>
