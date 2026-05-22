<?php
$nums = [1, 2];
$nums[1] = 3;
echo (is_float($nums[1]) ? json_encode($nums[1], 1344) : $nums[1]), PHP_EOL;
