<?php
echo rtrim(str_replace(":", ": ", str_replace(",", ", ", json_encode(array_slice([1, 2, 3], 1, 3 - 1), 320)))), PHP_EOL;
echo rtrim(str_replace(":", ": ", str_replace(",", ", ", json_encode(array_slice([1, 2, 3], 0, 2 - 0), 320)))), PHP_EOL;
echo rtrim(substr("hello", 1, 4 - 1)), PHP_EOL;
?>
