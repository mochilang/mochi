<?php
$a = [1, 2];
echo rtrim(implode(" ", array_map("json_encode", array_merge($a, [3])))), PHP_EOL;
?>
