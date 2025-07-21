<?php
$m = [1 => "a", 2 => "b"];
echo rtrim((is_float($m[1]) ? sprintf("%.15f", $m[1]) : $m[1])), PHP_EOL;
?>
