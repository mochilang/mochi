<?php
$a = 10 - 3;
$b = 2 + 2;
echo rtrim((is_float($a) ? sprintf("%.15f", $a) : $a)), PHP_EOL;
echo rtrim(($a == 7 ? "true" : "false")), PHP_EOL;
echo rtrim(($b < 5 ? "true" : "false")), PHP_EOL;
?>
