<?php
$matrix = [[1, 2], [3, 4]];
$matrix[1][0] = 5;
echo (is_float($matrix[1][0]) ? json_encode($matrix[1][0], 1344) : $matrix[1][0]), PHP_EOL;
