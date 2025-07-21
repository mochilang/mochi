<?php
$xs = [1, 2, 3];
echo rtrim((in_array(2, $xs) ? "true" : "false")), PHP_EOL;
echo rtrim(!(in_array(5, $xs))), PHP_EOL;
?>
