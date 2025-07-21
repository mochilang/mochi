<?php
$xs = [10, 20, 30];
echo rtrim((is_float($xs[1]) ? sprintf("%.15f", $xs[1]) : $xs[1])), PHP_EOL;
?>
