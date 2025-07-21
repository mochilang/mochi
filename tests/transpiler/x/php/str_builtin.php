<?php
echo rtrim((is_float(strval(123)) ? sprintf("%.15f", strval(123)) : strval(123))), PHP_EOL;
?>
