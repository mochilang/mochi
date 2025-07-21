<?php
$m = [1 => "a", 2 => "b"];
echo rtrim((is_float((array_key_exists(1, $m) ? 1 : 0)) ? sprintf("%.15f", (array_key_exists(1, $m) ? 1 : 0)) : (array_key_exists(1, $m) ? 1 : 0))), PHP_EOL;
echo rtrim((is_float((array_key_exists(3, $m) ? 1 : 0)) ? sprintf("%.15f", (array_key_exists(3, $m) ? 1 : 0)) : (array_key_exists(3, $m) ? 1 : 0))), PHP_EOL;
?>
