<?php
echo rtrim(implode(" ", array_map("json_encode", array_slice([1, 2, 3], 1, 3 - 1)))), PHP_EOL;
echo rtrim(implode(" ", array_map("json_encode", array_slice([1, 2, 3], 0, 2 - 0)))), PHP_EOL;
echo rtrim((is_float(substr("hello", 1, 4 - 1)) ? sprintf("%.15f", substr("hello", 1, 4 - 1)) : substr("hello", 1, 4 - 1))), PHP_EOL;
?>
