<?php
$nums = [3, 1, 4];
echo rtrim((is_float(min($nums)) ? sprintf("%.15f", min($nums)) : min($nums))), PHP_EOL;
echo rtrim((is_float(max($nums)) ? sprintf("%.15f", max($nums)) : max($nums))), PHP_EOL;
?>
