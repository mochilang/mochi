<?php
echo rtrim((is_float(array_sum([1, 2, 3]) / count([1, 2, 3])) ? sprintf("%.15f", array_sum([1, 2, 3]) / count([1, 2, 3])) : array_sum([1, 2, 3]) / count([1, 2, 3]))), PHP_EOL;
?>
