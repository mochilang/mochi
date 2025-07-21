<?php
$s = "catch";
echo rtrim((is_float((str_contains($s, "cat") ? 1 : 0)) ? sprintf("%.15f", (str_contains($s, "cat") ? 1 : 0)) : (str_contains($s, "cat") ? 1 : 0))), PHP_EOL;
echo rtrim((is_float((str_contains($s, "dog") ? 1 : 0)) ? sprintf("%.15f", (str_contains($s, "dog") ? 1 : 0)) : (str_contains($s, "dog") ? 1 : 0))), PHP_EOL;
?>
