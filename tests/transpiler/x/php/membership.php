<?php
$nums = [1, 2, 3];
echo rtrim((is_float((in_array(2, $nums) ? 1 : 0)) ? sprintf("%.15f", (in_array(2, $nums) ? 1 : 0)) : (in_array(2, $nums) ? 1 : 0))), PHP_EOL;
echo rtrim((is_float((in_array(4, $nums) ? 1 : 0)) ? sprintf("%.15f", (in_array(4, $nums) ? 1 : 0)) : (in_array(4, $nums) ? 1 : 0))), PHP_EOL;
?>
