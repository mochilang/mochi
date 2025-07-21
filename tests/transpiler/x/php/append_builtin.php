<?php
$a = [1, 2];
echo rtrim(str_replace(":", ": ", str_replace(",", ", ", json_encode(array_merge($a, [3]), 320)))), PHP_EOL;
?>
