<?php
$nums = [1, 2];
$nums[1] = 3;
echo rtrim((is_float($nums[1]) ? sprintf("%.15f", $nums[1]) : $nums[1])), PHP_EOL;
?>
