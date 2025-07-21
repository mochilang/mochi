<?php
$matrix = [[1, 2], [3, 4]];
$matrix[1][0] = 5;
echo rtrim((is_float($matrix[1][0]) ? sprintf("%.15f", $matrix[1][0]) : $matrix[1][0])), PHP_EOL;
?>
