<?php
echo "[" . implode(" ", array_slice([1, 2, 3], 1, 3 - 1)) . "]", PHP_EOL;
echo "[" . implode(" ", array_slice([1, 2, 3], 0, 2 - 0)) . "]", PHP_EOL;
echo substr("hello", 1, 4 - 1), PHP_EOL;
?>
