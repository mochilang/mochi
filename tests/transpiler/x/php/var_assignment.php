<?php
$x = 1;
$x = 2;
echo rtrim((is_float($x) ? sprintf("%.15f", $x) : $x)), PHP_EOL;
?>
