<?php
echo rtrim((is_float(1 + 2 * 3) ? sprintf("%.15f", 1 + 2 * 3) : 1 + 2 * 3)), PHP_EOL;
echo rtrim((is_float((1 + 2) * 3) ? sprintf("%.15f", (1 + 2) * 3) : (1 + 2) * 3)), PHP_EOL;
echo rtrim((is_float(2 * 3 + 1) ? sprintf("%.15f", 2 * 3 + 1) : 2 * 3 + 1)), PHP_EOL;
echo rtrim((is_float(2 * (3 + 1)) ? sprintf("%.15f", 2 * (3 + 1)) : 2 * (3 + 1))), PHP_EOL;
?>
