<?php
echo str_replace("false", "False", str_replace("true", "True", str_replace("\"", "'", str_replace(":", ": ", str_replace(",", ", ", json_encode(array_slice([1, 2, 3], 1, 3 - 1), 1344)))))), PHP_EOL;
echo str_replace("false", "False", str_replace("true", "True", str_replace("\"", "'", str_replace(":", ": ", str_replace(",", ", ", json_encode(array_slice([1, 2, 3], 0, 2 - 0), 1344)))))), PHP_EOL;
echo (is_float(substr("hello", 1, 4 - 1)) ? json_encode(substr("hello", 1, 4 - 1), 1344) : substr("hello", 1, 4 - 1)), PHP_EOL;
