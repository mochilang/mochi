<?php
$nums = [3, 1, 4];
echo (is_float(min($nums)) ? json_encode(min($nums), 1344) : min($nums)), PHP_EOL;
echo (is_float(max($nums)) ? json_encode(max($nums), 1344) : max($nums)), PHP_EOL;
