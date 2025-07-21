<?php
$a = 10 - 3;
$b = 2 + 2;
echo rtrim((is_float($a) ? sprintf("%.15f", $a) : $a)), PHP_EOL;
echo rtrim((is_float(($a == 7 ? 1 : 0)) ? sprintf("%.15f", ($a == 7 ? 1 : 0)) : ($a == 7 ? 1 : 0))), PHP_EOL;
echo rtrim((is_float(($b < 5 ? 1 : 0)) ? sprintf("%.15f", ($b < 5 ? 1 : 0)) : ($b < 5 ? 1 : 0))), PHP_EOL;
?>
