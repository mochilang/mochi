<?php
echo [1, 2] union [2, 3], PHP_EOL;
echo [1, 2, 3] except [2], PHP_EOL;
echo [1, 2, 3] intersect [2, 4], PHP_EOL;
echo strlen([1, 2] union_all [2, 3]), PHP_EOL;
?>
