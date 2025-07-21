<?php
echo str_replace("false", "False", str_replace("true", "True", str_replace("\"", "'", str_replace(":", ": ", str_replace(",", ", ", json_encode(array_values(array_unique(array_merge([1, 2], [2, 3]), SORT_REGULAR)), 1344)))))), PHP_EOL;
echo str_replace("false", "False", str_replace("true", "True", str_replace("\"", "'", str_replace(":", ": ", str_replace(",", ", ", json_encode(array_values(array_diff([1, 2, 3], [2])), 1344)))))), PHP_EOL;
echo str_replace("false", "False", str_replace("true", "True", str_replace("\"", "'", str_replace(":", ": ", str_replace(",", ", ", json_encode(array_values(array_intersect([1, 2, 3], [2, 4])), 1344)))))), PHP_EOL;
echo (is_float(count(array_merge([1, 2], [2, 3]))) ? json_encode(count(array_merge([1, 2], [2, 3])), 1344) : count(array_merge([1, 2], [2, 3]))), PHP_EOL;
