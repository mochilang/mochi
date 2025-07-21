<?php
echo rtrim(str_replace(":", ": ", str_replace(",", ", ", json_encode(array_values(array_unique(array_merge([1, 2], [2, 3]), SORT_REGULAR)), 320)))), PHP_EOL;
echo rtrim(str_replace(":", ": ", str_replace(",", ", ", json_encode(array_values(array_diff([1, 2, 3], [2])), 320)))), PHP_EOL;
echo rtrim(str_replace(":", ": ", str_replace(",", ", ", json_encode(array_values(array_intersect([1, 2, 3], [2, 4])), 320)))), PHP_EOL;
echo rtrim((is_float(count(array_merge([1, 2], [2, 3]))) ? sprintf("%.15f", count(array_merge([1, 2], [2, 3]))) : count(array_merge([1, 2], [2, 3])))), PHP_EOL;
?>
