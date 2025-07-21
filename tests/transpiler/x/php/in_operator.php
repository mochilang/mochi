<?php
$xs = [1, 2, 3];
echo rtrim((in_array(2, $xs) ? "true" : "false")), PHP_EOL;
echo rtrim((is_float(!(in_array(5, $xs))) ? sprintf("%.15f", !(in_array(5, $xs))) : !(in_array(5, $xs)))), PHP_EOL;
?>
