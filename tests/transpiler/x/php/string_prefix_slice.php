<?php
$prefix = "fore";
$s1 = "forest";
echo rtrim((is_float((substr($s1, 0, strlen($prefix) - 0) == $prefix ? 1 : 0)) ? sprintf("%.15f", (substr($s1, 0, strlen($prefix) - 0) == $prefix ? 1 : 0)) : (substr($s1, 0, strlen($prefix) - 0) == $prefix ? 1 : 0))), PHP_EOL;
$s2 = "desert";
echo rtrim((is_float((substr($s2, 0, strlen($prefix) - 0) == $prefix ? 1 : 0)) ? sprintf("%.15f", (substr($s2, 0, strlen($prefix) - 0) == $prefix ? 1 : 0)) : (substr($s2, 0, strlen($prefix) - 0) == $prefix ? 1 : 0))), PHP_EOL;
?>
