<?php
$a = 10;
$b = 20;
echo rtrim((is_float($a + $b) ? sprintf("%.15f", $a + $b) : $a + $b)), PHP_EOL;
?>
