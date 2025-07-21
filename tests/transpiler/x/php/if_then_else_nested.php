<?php
$x = 8;
$msg = ($x > 10 ? "big" : ($x > 5 ? "medium" : "small"));
echo rtrim($msg), PHP_EOL;
?>
