<?php
echo rtrim((is_float(count([1, 2, 3])) ? sprintf("%.15f", count([1, 2, 3])) : count([1, 2, 3]))), PHP_EOL;
?>
