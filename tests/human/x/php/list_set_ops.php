<?php
$a = [1,2];
$b = [2,3];
var_dump(array_values(array_unique(array_merge($a, $b))));
var_dump(array_values(array_diff([1,2,3], [2])));
var_dump(array_values(array_intersect([1,2,3], [2,4])));
var_dump(count(array_merge([1,2],[2,3])));
?>
