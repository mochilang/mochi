<?php
$x = 0;
echo rtrim((is_float($x) ? sprintf("%.15f", $x) : $x)), PHP_EOL;
?>
