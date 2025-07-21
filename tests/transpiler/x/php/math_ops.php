<?php
echo rtrim((is_float(6 * 7) ? sprintf("%.15f", 6 * 7) : 6 * 7)), PHP_EOL;
echo rtrim((is_float(intdiv(7, 2)) ? sprintf("%.15f", intdiv(7, 2)) : intdiv(7, 2))), PHP_EOL;
echo rtrim((is_float(7 % 2) ? sprintf("%.15f", 7 % 2) : 7 % 2)), PHP_EOL;
?>
