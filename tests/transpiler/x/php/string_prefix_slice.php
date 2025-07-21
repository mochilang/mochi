<?php
$prefix = "fore";
$s1 = "forest";
echo rtrim((substr($s1, 0, strlen($prefix) - 0) == $prefix ? "true" : "false")), PHP_EOL;
$s2 = "desert";
echo rtrim((substr($s2, 0, strlen($prefix) - 0) == $prefix ? "true" : "false")), PHP_EOL;
?>
