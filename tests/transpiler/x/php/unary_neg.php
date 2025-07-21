<?php
echo rtrim((is_float(-3) ? sprintf("%.15f", -3) : -3)), PHP_EOL;
echo rtrim((is_float(5 + (-2)) ? sprintf("%.15f", 5 + (-2)) : 5 + (-2))), PHP_EOL;
?>
